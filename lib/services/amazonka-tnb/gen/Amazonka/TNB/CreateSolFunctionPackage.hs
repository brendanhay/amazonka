{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.TNB.CreateSolFunctionPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a function package.
--
-- A function package is a .zip file in CSAR (Cloud Service Archive) format
-- that contains a network function (an ETSI standard telecommunication
-- application) and function package descriptor that uses the TOSCA
-- standard to describe how the network functions should run on your
-- network. For more information, see
-- <https://docs.aws.amazon.com/tnb/latest/ug/function-packages.html Function packages>
-- in the /Amazon Web Services Telco Network Builder User Guide/.
--
-- Creating a function package is the first step for creating a network in
-- AWS TNB. This request creates an empty container with an ID. The next
-- step is to upload the actual CSAR zip file into that empty container. To
-- upload function package content, see
-- <https://docs.aws.amazon.com/tnb/latest/APIReference/API_PutSolFunctionPackageContent.html PutSolFunctionPackageContent>.
module Amazonka.TNB.CreateSolFunctionPackage
  ( -- * Creating a Request
    CreateSolFunctionPackage (..),
    newCreateSolFunctionPackage,

    -- * Request Lenses
    createSolFunctionPackage_tags,

    -- * Destructuring the Response
    CreateSolFunctionPackageResponse (..),
    newCreateSolFunctionPackageResponse,

    -- * Response Lenses
    createSolFunctionPackageResponse_tags,
    createSolFunctionPackageResponse_httpStatus,
    createSolFunctionPackageResponse_arn,
    createSolFunctionPackageResponse_id,
    createSolFunctionPackageResponse_onboardingState,
    createSolFunctionPackageResponse_operationalState,
    createSolFunctionPackageResponse_usageState,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newCreateSolFunctionPackage' smart constructor.
data CreateSolFunctionPackage = CreateSolFunctionPackage'
  { -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. You can use tags to
    -- search and filter your resources or track your Amazon Web Services
    -- costs.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text))
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSolFunctionPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSolFunctionPackage_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
newCreateSolFunctionPackage ::
  CreateSolFunctionPackage
newCreateSolFunctionPackage =
  CreateSolFunctionPackage' {tags = Prelude.Nothing}

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
createSolFunctionPackage_tags :: Lens.Lens' CreateSolFunctionPackage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSolFunctionPackage_tags = Lens.lens (\CreateSolFunctionPackage' {tags} -> tags) (\s@CreateSolFunctionPackage' {} a -> s {tags = a} :: CreateSolFunctionPackage) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

instance Core.AWSRequest CreateSolFunctionPackage where
  type
    AWSResponse CreateSolFunctionPackage =
      CreateSolFunctionPackageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSolFunctionPackageResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "onboardingState")
            Prelude.<*> (x Data..:> "operationalState")
            Prelude.<*> (x Data..:> "usageState")
      )

instance Prelude.Hashable CreateSolFunctionPackage where
  hashWithSalt _salt CreateSolFunctionPackage' {..} =
    _salt `Prelude.hashWithSalt` tags

instance Prelude.NFData CreateSolFunctionPackage where
  rnf CreateSolFunctionPackage' {..} = Prelude.rnf tags

instance Data.ToHeaders CreateSolFunctionPackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSolFunctionPackage where
  toJSON CreateSolFunctionPackage' {..} =
    Data.object
      ( Prelude.catMaybes
          [("tags" Data..=) Prelude.<$> tags]
      )

instance Data.ToPath CreateSolFunctionPackage where
  toPath = Prelude.const "/sol/vnfpkgm/v1/vnf_packages"

instance Data.ToQuery CreateSolFunctionPackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSolFunctionPackageResponse' smart constructor.
data CreateSolFunctionPackageResponse = CreateSolFunctionPackageResponse'
  { -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. You can use tags to
    -- search and filter your resources or track your Amazon Web Services
    -- costs.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Function package ARN.
    arn :: Prelude.Text,
    -- | ID of the function package.
    id :: Prelude.Text,
    -- | Onboarding state of the function package.
    onboardingState :: OnboardingState,
    -- | Operational state of the function package.
    operationalState :: OperationalState,
    -- | Usage state of the function package.
    usageState :: UsageState
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSolFunctionPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSolFunctionPackageResponse_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
--
-- 'httpStatus', 'createSolFunctionPackageResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createSolFunctionPackageResponse_arn' - Function package ARN.
--
-- 'id', 'createSolFunctionPackageResponse_id' - ID of the function package.
--
-- 'onboardingState', 'createSolFunctionPackageResponse_onboardingState' - Onboarding state of the function package.
--
-- 'operationalState', 'createSolFunctionPackageResponse_operationalState' - Operational state of the function package.
--
-- 'usageState', 'createSolFunctionPackageResponse_usageState' - Usage state of the function package.
newCreateSolFunctionPackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'onboardingState'
  OnboardingState ->
  -- | 'operationalState'
  OperationalState ->
  -- | 'usageState'
  UsageState ->
  CreateSolFunctionPackageResponse
newCreateSolFunctionPackageResponse
  pHttpStatus_
  pArn_
  pId_
  pOnboardingState_
  pOperationalState_
  pUsageState_ =
    CreateSolFunctionPackageResponse'
      { tags =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        id = pId_,
        onboardingState = pOnboardingState_,
        operationalState = pOperationalState_,
        usageState = pUsageState_
      }

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
createSolFunctionPackageResponse_tags :: Lens.Lens' CreateSolFunctionPackageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSolFunctionPackageResponse_tags = Lens.lens (\CreateSolFunctionPackageResponse' {tags} -> tags) (\s@CreateSolFunctionPackageResponse' {} a -> s {tags = a} :: CreateSolFunctionPackageResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
createSolFunctionPackageResponse_httpStatus :: Lens.Lens' CreateSolFunctionPackageResponse Prelude.Int
createSolFunctionPackageResponse_httpStatus = Lens.lens (\CreateSolFunctionPackageResponse' {httpStatus} -> httpStatus) (\s@CreateSolFunctionPackageResponse' {} a -> s {httpStatus = a} :: CreateSolFunctionPackageResponse)

-- | Function package ARN.
createSolFunctionPackageResponse_arn :: Lens.Lens' CreateSolFunctionPackageResponse Prelude.Text
createSolFunctionPackageResponse_arn = Lens.lens (\CreateSolFunctionPackageResponse' {arn} -> arn) (\s@CreateSolFunctionPackageResponse' {} a -> s {arn = a} :: CreateSolFunctionPackageResponse)

-- | ID of the function package.
createSolFunctionPackageResponse_id :: Lens.Lens' CreateSolFunctionPackageResponse Prelude.Text
createSolFunctionPackageResponse_id = Lens.lens (\CreateSolFunctionPackageResponse' {id} -> id) (\s@CreateSolFunctionPackageResponse' {} a -> s {id = a} :: CreateSolFunctionPackageResponse)

-- | Onboarding state of the function package.
createSolFunctionPackageResponse_onboardingState :: Lens.Lens' CreateSolFunctionPackageResponse OnboardingState
createSolFunctionPackageResponse_onboardingState = Lens.lens (\CreateSolFunctionPackageResponse' {onboardingState} -> onboardingState) (\s@CreateSolFunctionPackageResponse' {} a -> s {onboardingState = a} :: CreateSolFunctionPackageResponse)

-- | Operational state of the function package.
createSolFunctionPackageResponse_operationalState :: Lens.Lens' CreateSolFunctionPackageResponse OperationalState
createSolFunctionPackageResponse_operationalState = Lens.lens (\CreateSolFunctionPackageResponse' {operationalState} -> operationalState) (\s@CreateSolFunctionPackageResponse' {} a -> s {operationalState = a} :: CreateSolFunctionPackageResponse)

-- | Usage state of the function package.
createSolFunctionPackageResponse_usageState :: Lens.Lens' CreateSolFunctionPackageResponse UsageState
createSolFunctionPackageResponse_usageState = Lens.lens (\CreateSolFunctionPackageResponse' {usageState} -> usageState) (\s@CreateSolFunctionPackageResponse' {} a -> s {usageState = a} :: CreateSolFunctionPackageResponse)

instance
  Prelude.NFData
    CreateSolFunctionPackageResponse
  where
  rnf CreateSolFunctionPackageResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf onboardingState
      `Prelude.seq` Prelude.rnf operationalState
      `Prelude.seq` Prelude.rnf usageState
