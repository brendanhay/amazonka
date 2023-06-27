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
-- Module      : Amazonka.TNB.CreateSolNetworkPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network package.
--
-- A network package is a .zip file in CSAR (Cloud Service Archive) format
-- defines the function packages you want to deploy and the Amazon Web
-- Services infrastructure you want to deploy them on. For more
-- information, see
-- <https://docs.aws.amazon.com/tnb/latest/ug/network-instances.html Network instances>
-- in the /Amazon Web Services Telco Network Builder User Guide/.
--
-- A network package consists of a network service descriptor (NSD) file
-- (required) and any additional files (optional), such as scripts specific
-- to your needs. For example, if you have multiple function packages in
-- your network package, you can use the NSD to define which network
-- functions should run in certain VPCs, subnets, or EKS clusters.
--
-- This request creates an empty network package container with an ID. Once
-- you create a network package, you can upload the network package content
-- using
-- <https://docs.aws.amazon.com/tnb/latest/APIReference/API_PutSolNetworkPackageContent.html PutSolNetworkPackageContent>.
module Amazonka.TNB.CreateSolNetworkPackage
  ( -- * Creating a Request
    CreateSolNetworkPackage (..),
    newCreateSolNetworkPackage,

    -- * Request Lenses
    createSolNetworkPackage_tags,

    -- * Destructuring the Response
    CreateSolNetworkPackageResponse (..),
    newCreateSolNetworkPackageResponse,

    -- * Response Lenses
    createSolNetworkPackageResponse_tags,
    createSolNetworkPackageResponse_httpStatus,
    createSolNetworkPackageResponse_arn,
    createSolNetworkPackageResponse_id,
    createSolNetworkPackageResponse_nsdOnboardingState,
    createSolNetworkPackageResponse_nsdOperationalState,
    createSolNetworkPackageResponse_nsdUsageState,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newCreateSolNetworkPackage' smart constructor.
data CreateSolNetworkPackage = CreateSolNetworkPackage'
  { -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. You can use tags to
    -- search and filter your resources or track your Amazon Web Services
    -- costs.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text))
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSolNetworkPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSolNetworkPackage_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
newCreateSolNetworkPackage ::
  CreateSolNetworkPackage
newCreateSolNetworkPackage =
  CreateSolNetworkPackage' {tags = Prelude.Nothing}

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
createSolNetworkPackage_tags :: Lens.Lens' CreateSolNetworkPackage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSolNetworkPackage_tags = Lens.lens (\CreateSolNetworkPackage' {tags} -> tags) (\s@CreateSolNetworkPackage' {} a -> s {tags = a} :: CreateSolNetworkPackage) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

instance Core.AWSRequest CreateSolNetworkPackage where
  type
    AWSResponse CreateSolNetworkPackage =
      CreateSolNetworkPackageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSolNetworkPackageResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "nsdOnboardingState")
            Prelude.<*> (x Data..:> "nsdOperationalState")
            Prelude.<*> (x Data..:> "nsdUsageState")
      )

instance Prelude.Hashable CreateSolNetworkPackage where
  hashWithSalt _salt CreateSolNetworkPackage' {..} =
    _salt `Prelude.hashWithSalt` tags

instance Prelude.NFData CreateSolNetworkPackage where
  rnf CreateSolNetworkPackage' {..} = Prelude.rnf tags

instance Data.ToHeaders CreateSolNetworkPackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSolNetworkPackage where
  toJSON CreateSolNetworkPackage' {..} =
    Data.object
      ( Prelude.catMaybes
          [("tags" Data..=) Prelude.<$> tags]
      )

instance Data.ToPath CreateSolNetworkPackage where
  toPath = Prelude.const "/sol/nsd/v1/ns_descriptors"

instance Data.ToQuery CreateSolNetworkPackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSolNetworkPackageResponse' smart constructor.
data CreateSolNetworkPackageResponse = CreateSolNetworkPackageResponse'
  { -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. You can use tags to
    -- search and filter your resources or track your Amazon Web Services
    -- costs.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Network package ARN.
    arn :: Prelude.Text,
    -- | ID of the network package.
    id :: Prelude.Text,
    -- | Onboarding state of the network service descriptor in the network
    -- package.
    nsdOnboardingState :: NsdOnboardingState,
    -- | Operational state of the network service descriptor in the network
    -- package.
    nsdOperationalState :: NsdOperationalState,
    -- | Usage state of the network service descriptor in the network package.
    nsdUsageState :: NsdUsageState
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSolNetworkPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSolNetworkPackageResponse_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
--
-- 'httpStatus', 'createSolNetworkPackageResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createSolNetworkPackageResponse_arn' - Network package ARN.
--
-- 'id', 'createSolNetworkPackageResponse_id' - ID of the network package.
--
-- 'nsdOnboardingState', 'createSolNetworkPackageResponse_nsdOnboardingState' - Onboarding state of the network service descriptor in the network
-- package.
--
-- 'nsdOperationalState', 'createSolNetworkPackageResponse_nsdOperationalState' - Operational state of the network service descriptor in the network
-- package.
--
-- 'nsdUsageState', 'createSolNetworkPackageResponse_nsdUsageState' - Usage state of the network service descriptor in the network package.
newCreateSolNetworkPackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'nsdOnboardingState'
  NsdOnboardingState ->
  -- | 'nsdOperationalState'
  NsdOperationalState ->
  -- | 'nsdUsageState'
  NsdUsageState ->
  CreateSolNetworkPackageResponse
newCreateSolNetworkPackageResponse
  pHttpStatus_
  pArn_
  pId_
  pNsdOnboardingState_
  pNsdOperationalState_
  pNsdUsageState_ =
    CreateSolNetworkPackageResponse'
      { tags =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        id = pId_,
        nsdOnboardingState = pNsdOnboardingState_,
        nsdOperationalState =
          pNsdOperationalState_,
        nsdUsageState = pNsdUsageState_
      }

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
createSolNetworkPackageResponse_tags :: Lens.Lens' CreateSolNetworkPackageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSolNetworkPackageResponse_tags = Lens.lens (\CreateSolNetworkPackageResponse' {tags} -> tags) (\s@CreateSolNetworkPackageResponse' {} a -> s {tags = a} :: CreateSolNetworkPackageResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
createSolNetworkPackageResponse_httpStatus :: Lens.Lens' CreateSolNetworkPackageResponse Prelude.Int
createSolNetworkPackageResponse_httpStatus = Lens.lens (\CreateSolNetworkPackageResponse' {httpStatus} -> httpStatus) (\s@CreateSolNetworkPackageResponse' {} a -> s {httpStatus = a} :: CreateSolNetworkPackageResponse)

-- | Network package ARN.
createSolNetworkPackageResponse_arn :: Lens.Lens' CreateSolNetworkPackageResponse Prelude.Text
createSolNetworkPackageResponse_arn = Lens.lens (\CreateSolNetworkPackageResponse' {arn} -> arn) (\s@CreateSolNetworkPackageResponse' {} a -> s {arn = a} :: CreateSolNetworkPackageResponse)

-- | ID of the network package.
createSolNetworkPackageResponse_id :: Lens.Lens' CreateSolNetworkPackageResponse Prelude.Text
createSolNetworkPackageResponse_id = Lens.lens (\CreateSolNetworkPackageResponse' {id} -> id) (\s@CreateSolNetworkPackageResponse' {} a -> s {id = a} :: CreateSolNetworkPackageResponse)

-- | Onboarding state of the network service descriptor in the network
-- package.
createSolNetworkPackageResponse_nsdOnboardingState :: Lens.Lens' CreateSolNetworkPackageResponse NsdOnboardingState
createSolNetworkPackageResponse_nsdOnboardingState = Lens.lens (\CreateSolNetworkPackageResponse' {nsdOnboardingState} -> nsdOnboardingState) (\s@CreateSolNetworkPackageResponse' {} a -> s {nsdOnboardingState = a} :: CreateSolNetworkPackageResponse)

-- | Operational state of the network service descriptor in the network
-- package.
createSolNetworkPackageResponse_nsdOperationalState :: Lens.Lens' CreateSolNetworkPackageResponse NsdOperationalState
createSolNetworkPackageResponse_nsdOperationalState = Lens.lens (\CreateSolNetworkPackageResponse' {nsdOperationalState} -> nsdOperationalState) (\s@CreateSolNetworkPackageResponse' {} a -> s {nsdOperationalState = a} :: CreateSolNetworkPackageResponse)

-- | Usage state of the network service descriptor in the network package.
createSolNetworkPackageResponse_nsdUsageState :: Lens.Lens' CreateSolNetworkPackageResponse NsdUsageState
createSolNetworkPackageResponse_nsdUsageState = Lens.lens (\CreateSolNetworkPackageResponse' {nsdUsageState} -> nsdUsageState) (\s@CreateSolNetworkPackageResponse' {} a -> s {nsdUsageState = a} :: CreateSolNetworkPackageResponse)

instance
  Prelude.NFData
    CreateSolNetworkPackageResponse
  where
  rnf CreateSolNetworkPackageResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf nsdOnboardingState
      `Prelude.seq` Prelude.rnf nsdOperationalState
      `Prelude.seq` Prelude.rnf nsdUsageState
