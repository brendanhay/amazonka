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
-- Module      : Amazonka.TNB.GetSolFunctionPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of an individual function package, such as the
-- operational state and whether the package is in use.
--
-- A function package is a .zip file in CSAR (Cloud Service Archive) format
-- that contains a network function (an ETSI standard telecommunication
-- application) and function package descriptor that uses the TOSCA
-- standard to describe how the network functions should run on your
-- network..
module Amazonka.TNB.GetSolFunctionPackage
  ( -- * Creating a Request
    GetSolFunctionPackage (..),
    newGetSolFunctionPackage,

    -- * Request Lenses
    getSolFunctionPackage_vnfPkgId,

    -- * Destructuring the Response
    GetSolFunctionPackageResponse (..),
    newGetSolFunctionPackageResponse,

    -- * Response Lenses
    getSolFunctionPackageResponse_metadata,
    getSolFunctionPackageResponse_tags,
    getSolFunctionPackageResponse_vnfProductName,
    getSolFunctionPackageResponse_vnfProvider,
    getSolFunctionPackageResponse_vnfdId,
    getSolFunctionPackageResponse_vnfdVersion,
    getSolFunctionPackageResponse_httpStatus,
    getSolFunctionPackageResponse_arn,
    getSolFunctionPackageResponse_id,
    getSolFunctionPackageResponse_onboardingState,
    getSolFunctionPackageResponse_operationalState,
    getSolFunctionPackageResponse_usageState,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newGetSolFunctionPackage' smart constructor.
data GetSolFunctionPackage = GetSolFunctionPackage'
  { -- | ID of the function package.
    vnfPkgId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolFunctionPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vnfPkgId', 'getSolFunctionPackage_vnfPkgId' - ID of the function package.
newGetSolFunctionPackage ::
  -- | 'vnfPkgId'
  Prelude.Text ->
  GetSolFunctionPackage
newGetSolFunctionPackage pVnfPkgId_ =
  GetSolFunctionPackage' {vnfPkgId = pVnfPkgId_}

-- | ID of the function package.
getSolFunctionPackage_vnfPkgId :: Lens.Lens' GetSolFunctionPackage Prelude.Text
getSolFunctionPackage_vnfPkgId = Lens.lens (\GetSolFunctionPackage' {vnfPkgId} -> vnfPkgId) (\s@GetSolFunctionPackage' {} a -> s {vnfPkgId = a} :: GetSolFunctionPackage)

instance Core.AWSRequest GetSolFunctionPackage where
  type
    AWSResponse GetSolFunctionPackage =
      GetSolFunctionPackageResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSolFunctionPackageResponse'
            Prelude.<$> (x Data..?> "metadata")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "vnfProductName")
            Prelude.<*> (x Data..?> "vnfProvider")
            Prelude.<*> (x Data..?> "vnfdId")
            Prelude.<*> (x Data..?> "vnfdVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "onboardingState")
            Prelude.<*> (x Data..:> "operationalState")
            Prelude.<*> (x Data..:> "usageState")
      )

instance Prelude.Hashable GetSolFunctionPackage where
  hashWithSalt _salt GetSolFunctionPackage' {..} =
    _salt `Prelude.hashWithSalt` vnfPkgId

instance Prelude.NFData GetSolFunctionPackage where
  rnf GetSolFunctionPackage' {..} = Prelude.rnf vnfPkgId

instance Data.ToHeaders GetSolFunctionPackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSolFunctionPackage where
  toPath GetSolFunctionPackage' {..} =
    Prelude.mconcat
      ["/sol/vnfpkgm/v1/vnf_packages/", Data.toBS vnfPkgId]

instance Data.ToQuery GetSolFunctionPackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSolFunctionPackageResponse' smart constructor.
data GetSolFunctionPackageResponse = GetSolFunctionPackageResponse'
  { metadata :: Prelude.Maybe GetSolFunctionPackageMetadata,
    -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. You can use tags to
    -- search and filter your resources or track your Amazon Web Services
    -- costs.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Network function product name.
    vnfProductName :: Prelude.Maybe Prelude.Text,
    -- | Network function provider.
    vnfProvider :: Prelude.Maybe Prelude.Text,
    -- | Function package descriptor ID.
    vnfdId :: Prelude.Maybe Prelude.Text,
    -- | Function package descriptor version.
    vnfdVersion :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Function package ARN.
    arn :: Prelude.Text,
    -- | Function package ID.
    id :: Prelude.Text,
    -- | Function package onboarding state.
    onboardingState :: OnboardingState,
    -- | Function package operational state.
    operationalState :: OperationalState,
    -- | Function package usage state.
    usageState :: UsageState
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolFunctionPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'getSolFunctionPackageResponse_metadata' - Undocumented member.
--
-- 'tags', 'getSolFunctionPackageResponse_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
--
-- 'vnfProductName', 'getSolFunctionPackageResponse_vnfProductName' - Network function product name.
--
-- 'vnfProvider', 'getSolFunctionPackageResponse_vnfProvider' - Network function provider.
--
-- 'vnfdId', 'getSolFunctionPackageResponse_vnfdId' - Function package descriptor ID.
--
-- 'vnfdVersion', 'getSolFunctionPackageResponse_vnfdVersion' - Function package descriptor version.
--
-- 'httpStatus', 'getSolFunctionPackageResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getSolFunctionPackageResponse_arn' - Function package ARN.
--
-- 'id', 'getSolFunctionPackageResponse_id' - Function package ID.
--
-- 'onboardingState', 'getSolFunctionPackageResponse_onboardingState' - Function package onboarding state.
--
-- 'operationalState', 'getSolFunctionPackageResponse_operationalState' - Function package operational state.
--
-- 'usageState', 'getSolFunctionPackageResponse_usageState' - Function package usage state.
newGetSolFunctionPackageResponse ::
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
  GetSolFunctionPackageResponse
newGetSolFunctionPackageResponse
  pHttpStatus_
  pArn_
  pId_
  pOnboardingState_
  pOperationalState_
  pUsageState_ =
    GetSolFunctionPackageResponse'
      { metadata =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        vnfProductName = Prelude.Nothing,
        vnfProvider = Prelude.Nothing,
        vnfdId = Prelude.Nothing,
        vnfdVersion = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        id = pId_,
        onboardingState = pOnboardingState_,
        operationalState = pOperationalState_,
        usageState = pUsageState_
      }

-- | Undocumented member.
getSolFunctionPackageResponse_metadata :: Lens.Lens' GetSolFunctionPackageResponse (Prelude.Maybe GetSolFunctionPackageMetadata)
getSolFunctionPackageResponse_metadata = Lens.lens (\GetSolFunctionPackageResponse' {metadata} -> metadata) (\s@GetSolFunctionPackageResponse' {} a -> s {metadata = a} :: GetSolFunctionPackageResponse)

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
getSolFunctionPackageResponse_tags :: Lens.Lens' GetSolFunctionPackageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getSolFunctionPackageResponse_tags = Lens.lens (\GetSolFunctionPackageResponse' {tags} -> tags) (\s@GetSolFunctionPackageResponse' {} a -> s {tags = a} :: GetSolFunctionPackageResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Network function product name.
getSolFunctionPackageResponse_vnfProductName :: Lens.Lens' GetSolFunctionPackageResponse (Prelude.Maybe Prelude.Text)
getSolFunctionPackageResponse_vnfProductName = Lens.lens (\GetSolFunctionPackageResponse' {vnfProductName} -> vnfProductName) (\s@GetSolFunctionPackageResponse' {} a -> s {vnfProductName = a} :: GetSolFunctionPackageResponse)

-- | Network function provider.
getSolFunctionPackageResponse_vnfProvider :: Lens.Lens' GetSolFunctionPackageResponse (Prelude.Maybe Prelude.Text)
getSolFunctionPackageResponse_vnfProvider = Lens.lens (\GetSolFunctionPackageResponse' {vnfProvider} -> vnfProvider) (\s@GetSolFunctionPackageResponse' {} a -> s {vnfProvider = a} :: GetSolFunctionPackageResponse)

-- | Function package descriptor ID.
getSolFunctionPackageResponse_vnfdId :: Lens.Lens' GetSolFunctionPackageResponse (Prelude.Maybe Prelude.Text)
getSolFunctionPackageResponse_vnfdId = Lens.lens (\GetSolFunctionPackageResponse' {vnfdId} -> vnfdId) (\s@GetSolFunctionPackageResponse' {} a -> s {vnfdId = a} :: GetSolFunctionPackageResponse)

-- | Function package descriptor version.
getSolFunctionPackageResponse_vnfdVersion :: Lens.Lens' GetSolFunctionPackageResponse (Prelude.Maybe Prelude.Text)
getSolFunctionPackageResponse_vnfdVersion = Lens.lens (\GetSolFunctionPackageResponse' {vnfdVersion} -> vnfdVersion) (\s@GetSolFunctionPackageResponse' {} a -> s {vnfdVersion = a} :: GetSolFunctionPackageResponse)

-- | The response's http status code.
getSolFunctionPackageResponse_httpStatus :: Lens.Lens' GetSolFunctionPackageResponse Prelude.Int
getSolFunctionPackageResponse_httpStatus = Lens.lens (\GetSolFunctionPackageResponse' {httpStatus} -> httpStatus) (\s@GetSolFunctionPackageResponse' {} a -> s {httpStatus = a} :: GetSolFunctionPackageResponse)

-- | Function package ARN.
getSolFunctionPackageResponse_arn :: Lens.Lens' GetSolFunctionPackageResponse Prelude.Text
getSolFunctionPackageResponse_arn = Lens.lens (\GetSolFunctionPackageResponse' {arn} -> arn) (\s@GetSolFunctionPackageResponse' {} a -> s {arn = a} :: GetSolFunctionPackageResponse)

-- | Function package ID.
getSolFunctionPackageResponse_id :: Lens.Lens' GetSolFunctionPackageResponse Prelude.Text
getSolFunctionPackageResponse_id = Lens.lens (\GetSolFunctionPackageResponse' {id} -> id) (\s@GetSolFunctionPackageResponse' {} a -> s {id = a} :: GetSolFunctionPackageResponse)

-- | Function package onboarding state.
getSolFunctionPackageResponse_onboardingState :: Lens.Lens' GetSolFunctionPackageResponse OnboardingState
getSolFunctionPackageResponse_onboardingState = Lens.lens (\GetSolFunctionPackageResponse' {onboardingState} -> onboardingState) (\s@GetSolFunctionPackageResponse' {} a -> s {onboardingState = a} :: GetSolFunctionPackageResponse)

-- | Function package operational state.
getSolFunctionPackageResponse_operationalState :: Lens.Lens' GetSolFunctionPackageResponse OperationalState
getSolFunctionPackageResponse_operationalState = Lens.lens (\GetSolFunctionPackageResponse' {operationalState} -> operationalState) (\s@GetSolFunctionPackageResponse' {} a -> s {operationalState = a} :: GetSolFunctionPackageResponse)

-- | Function package usage state.
getSolFunctionPackageResponse_usageState :: Lens.Lens' GetSolFunctionPackageResponse UsageState
getSolFunctionPackageResponse_usageState = Lens.lens (\GetSolFunctionPackageResponse' {usageState} -> usageState) (\s@GetSolFunctionPackageResponse' {} a -> s {usageState = a} :: GetSolFunctionPackageResponse)

instance Prelude.NFData GetSolFunctionPackageResponse where
  rnf GetSolFunctionPackageResponse' {..} =
    Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vnfProductName
      `Prelude.seq` Prelude.rnf vnfProvider
      `Prelude.seq` Prelude.rnf vnfdId
      `Prelude.seq` Prelude.rnf vnfdVersion
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf onboardingState
      `Prelude.seq` Prelude.rnf operationalState
      `Prelude.seq` Prelude.rnf usageState
