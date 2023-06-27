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
-- Module      : Amazonka.TNB.GetSolNetworkPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of a network package.
--
-- A network package is a .zip file in CSAR (Cloud Service Archive) format
-- defines the function packages you want to deploy and the Amazon Web
-- Services infrastructure you want to deploy them on.
module Amazonka.TNB.GetSolNetworkPackage
  ( -- * Creating a Request
    GetSolNetworkPackage (..),
    newGetSolNetworkPackage,

    -- * Request Lenses
    getSolNetworkPackage_nsdInfoId,

    -- * Destructuring the Response
    GetSolNetworkPackageResponse (..),
    newGetSolNetworkPackageResponse,

    -- * Response Lenses
    getSolNetworkPackageResponse_tags,
    getSolNetworkPackageResponse_httpStatus,
    getSolNetworkPackageResponse_arn,
    getSolNetworkPackageResponse_id,
    getSolNetworkPackageResponse_metadata,
    getSolNetworkPackageResponse_nsdId,
    getSolNetworkPackageResponse_nsdName,
    getSolNetworkPackageResponse_nsdOnboardingState,
    getSolNetworkPackageResponse_nsdOperationalState,
    getSolNetworkPackageResponse_nsdUsageState,
    getSolNetworkPackageResponse_nsdVersion,
    getSolNetworkPackageResponse_vnfPkgIds,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newGetSolNetworkPackage' smart constructor.
data GetSolNetworkPackage = GetSolNetworkPackage'
  { -- | ID of the network service descriptor in the network package.
    nsdInfoId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolNetworkPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nsdInfoId', 'getSolNetworkPackage_nsdInfoId' - ID of the network service descriptor in the network package.
newGetSolNetworkPackage ::
  -- | 'nsdInfoId'
  Prelude.Text ->
  GetSolNetworkPackage
newGetSolNetworkPackage pNsdInfoId_ =
  GetSolNetworkPackage' {nsdInfoId = pNsdInfoId_}

-- | ID of the network service descriptor in the network package.
getSolNetworkPackage_nsdInfoId :: Lens.Lens' GetSolNetworkPackage Prelude.Text
getSolNetworkPackage_nsdInfoId = Lens.lens (\GetSolNetworkPackage' {nsdInfoId} -> nsdInfoId) (\s@GetSolNetworkPackage' {} a -> s {nsdInfoId = a} :: GetSolNetworkPackage)

instance Core.AWSRequest GetSolNetworkPackage where
  type
    AWSResponse GetSolNetworkPackage =
      GetSolNetworkPackageResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSolNetworkPackageResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "metadata")
            Prelude.<*> (x Data..:> "nsdId")
            Prelude.<*> (x Data..:> "nsdName")
            Prelude.<*> (x Data..:> "nsdOnboardingState")
            Prelude.<*> (x Data..:> "nsdOperationalState")
            Prelude.<*> (x Data..:> "nsdUsageState")
            Prelude.<*> (x Data..:> "nsdVersion")
            Prelude.<*> (x Data..?> "vnfPkgIds" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable GetSolNetworkPackage where
  hashWithSalt _salt GetSolNetworkPackage' {..} =
    _salt `Prelude.hashWithSalt` nsdInfoId

instance Prelude.NFData GetSolNetworkPackage where
  rnf GetSolNetworkPackage' {..} = Prelude.rnf nsdInfoId

instance Data.ToHeaders GetSolNetworkPackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSolNetworkPackage where
  toPath GetSolNetworkPackage' {..} =
    Prelude.mconcat
      ["/sol/nsd/v1/ns_descriptors/", Data.toBS nsdInfoId]

instance Data.ToQuery GetSolNetworkPackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSolNetworkPackageResponse' smart constructor.
data GetSolNetworkPackageResponse = GetSolNetworkPackageResponse'
  { -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. You can use tags to
    -- search and filter your resources or track your Amazon Web Services
    -- costs.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Network package ARN.
    arn :: Prelude.Text,
    -- | Network package ID.
    id :: Prelude.Text,
    metadata :: GetSolNetworkPackageMetadata,
    -- | Network service descriptor ID.
    nsdId :: Prelude.Text,
    -- | Network service descriptor name.
    nsdName :: Prelude.Text,
    -- | Network service descriptor onboarding state.
    nsdOnboardingState :: NsdOnboardingState,
    -- | Network service descriptor operational state.
    nsdOperationalState :: NsdOperationalState,
    -- | Network service descriptor usage state.
    nsdUsageState :: NsdUsageState,
    -- | Network service descriptor version.
    nsdVersion :: Prelude.Text,
    -- | Identifies the function package for the function package descriptor
    -- referenced by the onboarded network package.
    vnfPkgIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolNetworkPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getSolNetworkPackageResponse_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
--
-- 'httpStatus', 'getSolNetworkPackageResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getSolNetworkPackageResponse_arn' - Network package ARN.
--
-- 'id', 'getSolNetworkPackageResponse_id' - Network package ID.
--
-- 'metadata', 'getSolNetworkPackageResponse_metadata' - Undocumented member.
--
-- 'nsdId', 'getSolNetworkPackageResponse_nsdId' - Network service descriptor ID.
--
-- 'nsdName', 'getSolNetworkPackageResponse_nsdName' - Network service descriptor name.
--
-- 'nsdOnboardingState', 'getSolNetworkPackageResponse_nsdOnboardingState' - Network service descriptor onboarding state.
--
-- 'nsdOperationalState', 'getSolNetworkPackageResponse_nsdOperationalState' - Network service descriptor operational state.
--
-- 'nsdUsageState', 'getSolNetworkPackageResponse_nsdUsageState' - Network service descriptor usage state.
--
-- 'nsdVersion', 'getSolNetworkPackageResponse_nsdVersion' - Network service descriptor version.
--
-- 'vnfPkgIds', 'getSolNetworkPackageResponse_vnfPkgIds' - Identifies the function package for the function package descriptor
-- referenced by the onboarded network package.
newGetSolNetworkPackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'metadata'
  GetSolNetworkPackageMetadata ->
  -- | 'nsdId'
  Prelude.Text ->
  -- | 'nsdName'
  Prelude.Text ->
  -- | 'nsdOnboardingState'
  NsdOnboardingState ->
  -- | 'nsdOperationalState'
  NsdOperationalState ->
  -- | 'nsdUsageState'
  NsdUsageState ->
  -- | 'nsdVersion'
  Prelude.Text ->
  GetSolNetworkPackageResponse
newGetSolNetworkPackageResponse
  pHttpStatus_
  pArn_
  pId_
  pMetadata_
  pNsdId_
  pNsdName_
  pNsdOnboardingState_
  pNsdOperationalState_
  pNsdUsageState_
  pNsdVersion_ =
    GetSolNetworkPackageResponse'
      { tags =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        id = pId_,
        metadata = pMetadata_,
        nsdId = pNsdId_,
        nsdName = pNsdName_,
        nsdOnboardingState = pNsdOnboardingState_,
        nsdOperationalState = pNsdOperationalState_,
        nsdUsageState = pNsdUsageState_,
        nsdVersion = pNsdVersion_,
        vnfPkgIds = Prelude.mempty
      }

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
getSolNetworkPackageResponse_tags :: Lens.Lens' GetSolNetworkPackageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getSolNetworkPackageResponse_tags = Lens.lens (\GetSolNetworkPackageResponse' {tags} -> tags) (\s@GetSolNetworkPackageResponse' {} a -> s {tags = a} :: GetSolNetworkPackageResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
getSolNetworkPackageResponse_httpStatus :: Lens.Lens' GetSolNetworkPackageResponse Prelude.Int
getSolNetworkPackageResponse_httpStatus = Lens.lens (\GetSolNetworkPackageResponse' {httpStatus} -> httpStatus) (\s@GetSolNetworkPackageResponse' {} a -> s {httpStatus = a} :: GetSolNetworkPackageResponse)

-- | Network package ARN.
getSolNetworkPackageResponse_arn :: Lens.Lens' GetSolNetworkPackageResponse Prelude.Text
getSolNetworkPackageResponse_arn = Lens.lens (\GetSolNetworkPackageResponse' {arn} -> arn) (\s@GetSolNetworkPackageResponse' {} a -> s {arn = a} :: GetSolNetworkPackageResponse)

-- | Network package ID.
getSolNetworkPackageResponse_id :: Lens.Lens' GetSolNetworkPackageResponse Prelude.Text
getSolNetworkPackageResponse_id = Lens.lens (\GetSolNetworkPackageResponse' {id} -> id) (\s@GetSolNetworkPackageResponse' {} a -> s {id = a} :: GetSolNetworkPackageResponse)

-- | Undocumented member.
getSolNetworkPackageResponse_metadata :: Lens.Lens' GetSolNetworkPackageResponse GetSolNetworkPackageMetadata
getSolNetworkPackageResponse_metadata = Lens.lens (\GetSolNetworkPackageResponse' {metadata} -> metadata) (\s@GetSolNetworkPackageResponse' {} a -> s {metadata = a} :: GetSolNetworkPackageResponse)

-- | Network service descriptor ID.
getSolNetworkPackageResponse_nsdId :: Lens.Lens' GetSolNetworkPackageResponse Prelude.Text
getSolNetworkPackageResponse_nsdId = Lens.lens (\GetSolNetworkPackageResponse' {nsdId} -> nsdId) (\s@GetSolNetworkPackageResponse' {} a -> s {nsdId = a} :: GetSolNetworkPackageResponse)

-- | Network service descriptor name.
getSolNetworkPackageResponse_nsdName :: Lens.Lens' GetSolNetworkPackageResponse Prelude.Text
getSolNetworkPackageResponse_nsdName = Lens.lens (\GetSolNetworkPackageResponse' {nsdName} -> nsdName) (\s@GetSolNetworkPackageResponse' {} a -> s {nsdName = a} :: GetSolNetworkPackageResponse)

-- | Network service descriptor onboarding state.
getSolNetworkPackageResponse_nsdOnboardingState :: Lens.Lens' GetSolNetworkPackageResponse NsdOnboardingState
getSolNetworkPackageResponse_nsdOnboardingState = Lens.lens (\GetSolNetworkPackageResponse' {nsdOnboardingState} -> nsdOnboardingState) (\s@GetSolNetworkPackageResponse' {} a -> s {nsdOnboardingState = a} :: GetSolNetworkPackageResponse)

-- | Network service descriptor operational state.
getSolNetworkPackageResponse_nsdOperationalState :: Lens.Lens' GetSolNetworkPackageResponse NsdOperationalState
getSolNetworkPackageResponse_nsdOperationalState = Lens.lens (\GetSolNetworkPackageResponse' {nsdOperationalState} -> nsdOperationalState) (\s@GetSolNetworkPackageResponse' {} a -> s {nsdOperationalState = a} :: GetSolNetworkPackageResponse)

-- | Network service descriptor usage state.
getSolNetworkPackageResponse_nsdUsageState :: Lens.Lens' GetSolNetworkPackageResponse NsdUsageState
getSolNetworkPackageResponse_nsdUsageState = Lens.lens (\GetSolNetworkPackageResponse' {nsdUsageState} -> nsdUsageState) (\s@GetSolNetworkPackageResponse' {} a -> s {nsdUsageState = a} :: GetSolNetworkPackageResponse)

-- | Network service descriptor version.
getSolNetworkPackageResponse_nsdVersion :: Lens.Lens' GetSolNetworkPackageResponse Prelude.Text
getSolNetworkPackageResponse_nsdVersion = Lens.lens (\GetSolNetworkPackageResponse' {nsdVersion} -> nsdVersion) (\s@GetSolNetworkPackageResponse' {} a -> s {nsdVersion = a} :: GetSolNetworkPackageResponse)

-- | Identifies the function package for the function package descriptor
-- referenced by the onboarded network package.
getSolNetworkPackageResponse_vnfPkgIds :: Lens.Lens' GetSolNetworkPackageResponse [Prelude.Text]
getSolNetworkPackageResponse_vnfPkgIds = Lens.lens (\GetSolNetworkPackageResponse' {vnfPkgIds} -> vnfPkgIds) (\s@GetSolNetworkPackageResponse' {} a -> s {vnfPkgIds = a} :: GetSolNetworkPackageResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetSolNetworkPackageResponse where
  rnf GetSolNetworkPackageResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf nsdId
      `Prelude.seq` Prelude.rnf nsdName
      `Prelude.seq` Prelude.rnf nsdOnboardingState
      `Prelude.seq` Prelude.rnf nsdOperationalState
      `Prelude.seq` Prelude.rnf nsdUsageState
      `Prelude.seq` Prelude.rnf nsdVersion
      `Prelude.seq` Prelude.rnf vnfPkgIds
