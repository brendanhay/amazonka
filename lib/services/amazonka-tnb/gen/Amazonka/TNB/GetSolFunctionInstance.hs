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
-- Module      : Amazonka.TNB.GetSolFunctionInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of a network function instance, including the
-- instantation state and metadata from the function package descriptor in
-- the network function package.
--
-- A network function instance is a function in a function package .
module Amazonka.TNB.GetSolFunctionInstance
  ( -- * Creating a Request
    GetSolFunctionInstance (..),
    newGetSolFunctionInstance,

    -- * Request Lenses
    getSolFunctionInstance_vnfInstanceId,

    -- * Destructuring the Response
    GetSolFunctionInstanceResponse (..),
    newGetSolFunctionInstanceResponse,

    -- * Response Lenses
    getSolFunctionInstanceResponse_instantiatedVnfInfo,
    getSolFunctionInstanceResponse_tags,
    getSolFunctionInstanceResponse_vnfProductName,
    getSolFunctionInstanceResponse_vnfProvider,
    getSolFunctionInstanceResponse_vnfdVersion,
    getSolFunctionInstanceResponse_httpStatus,
    getSolFunctionInstanceResponse_arn,
    getSolFunctionInstanceResponse_id,
    getSolFunctionInstanceResponse_instantiationState,
    getSolFunctionInstanceResponse_metadata,
    getSolFunctionInstanceResponse_nsInstanceId,
    getSolFunctionInstanceResponse_vnfPkgId,
    getSolFunctionInstanceResponse_vnfdId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newGetSolFunctionInstance' smart constructor.
data GetSolFunctionInstance = GetSolFunctionInstance'
  { -- | ID of the network function.
    vnfInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolFunctionInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vnfInstanceId', 'getSolFunctionInstance_vnfInstanceId' - ID of the network function.
newGetSolFunctionInstance ::
  -- | 'vnfInstanceId'
  Prelude.Text ->
  GetSolFunctionInstance
newGetSolFunctionInstance pVnfInstanceId_ =
  GetSolFunctionInstance'
    { vnfInstanceId =
        pVnfInstanceId_
    }

-- | ID of the network function.
getSolFunctionInstance_vnfInstanceId :: Lens.Lens' GetSolFunctionInstance Prelude.Text
getSolFunctionInstance_vnfInstanceId = Lens.lens (\GetSolFunctionInstance' {vnfInstanceId} -> vnfInstanceId) (\s@GetSolFunctionInstance' {} a -> s {vnfInstanceId = a} :: GetSolFunctionInstance)

instance Core.AWSRequest GetSolFunctionInstance where
  type
    AWSResponse GetSolFunctionInstance =
      GetSolFunctionInstanceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSolFunctionInstanceResponse'
            Prelude.<$> (x Data..?> "instantiatedVnfInfo")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "vnfProductName")
            Prelude.<*> (x Data..?> "vnfProvider")
            Prelude.<*> (x Data..?> "vnfdVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "instantiationState")
            Prelude.<*> (x Data..:> "metadata")
            Prelude.<*> (x Data..:> "nsInstanceId")
            Prelude.<*> (x Data..:> "vnfPkgId")
            Prelude.<*> (x Data..:> "vnfdId")
      )

instance Prelude.Hashable GetSolFunctionInstance where
  hashWithSalt _salt GetSolFunctionInstance' {..} =
    _salt `Prelude.hashWithSalt` vnfInstanceId

instance Prelude.NFData GetSolFunctionInstance where
  rnf GetSolFunctionInstance' {..} =
    Prelude.rnf vnfInstanceId

instance Data.ToHeaders GetSolFunctionInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSolFunctionInstance where
  toPath GetSolFunctionInstance' {..} =
    Prelude.mconcat
      [ "/sol/vnflcm/v1/vnf_instances/",
        Data.toBS vnfInstanceId
      ]

instance Data.ToQuery GetSolFunctionInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSolFunctionInstanceResponse' smart constructor.
data GetSolFunctionInstanceResponse = GetSolFunctionInstanceResponse'
  { instantiatedVnfInfo :: Prelude.Maybe GetSolVnfInfo,
    -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. You can use tags to
    -- search and filter your resources or track your Amazon Web Services
    -- costs.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Network function product name.
    vnfProductName :: Prelude.Maybe Prelude.Text,
    -- | Network function provider.
    vnfProvider :: Prelude.Maybe Prelude.Text,
    -- | Function package descriptor version.
    vnfdVersion :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Network function instance ARN.
    arn :: Prelude.Text,
    -- | Network function instance ID.
    id :: Prelude.Text,
    -- | Network function instantiation state.
    instantiationState :: VnfInstantiationState,
    metadata :: GetSolFunctionInstanceMetadata,
    -- | Network instance ID.
    nsInstanceId :: Prelude.Text,
    -- | Function package ID.
    vnfPkgId :: Prelude.Text,
    -- | Function package descriptor ID.
    vnfdId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolFunctionInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instantiatedVnfInfo', 'getSolFunctionInstanceResponse_instantiatedVnfInfo' - Undocumented member.
--
-- 'tags', 'getSolFunctionInstanceResponse_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
--
-- 'vnfProductName', 'getSolFunctionInstanceResponse_vnfProductName' - Network function product name.
--
-- 'vnfProvider', 'getSolFunctionInstanceResponse_vnfProvider' - Network function provider.
--
-- 'vnfdVersion', 'getSolFunctionInstanceResponse_vnfdVersion' - Function package descriptor version.
--
-- 'httpStatus', 'getSolFunctionInstanceResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getSolFunctionInstanceResponse_arn' - Network function instance ARN.
--
-- 'id', 'getSolFunctionInstanceResponse_id' - Network function instance ID.
--
-- 'instantiationState', 'getSolFunctionInstanceResponse_instantiationState' - Network function instantiation state.
--
-- 'metadata', 'getSolFunctionInstanceResponse_metadata' - Undocumented member.
--
-- 'nsInstanceId', 'getSolFunctionInstanceResponse_nsInstanceId' - Network instance ID.
--
-- 'vnfPkgId', 'getSolFunctionInstanceResponse_vnfPkgId' - Function package ID.
--
-- 'vnfdId', 'getSolFunctionInstanceResponse_vnfdId' - Function package descriptor ID.
newGetSolFunctionInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'instantiationState'
  VnfInstantiationState ->
  -- | 'metadata'
  GetSolFunctionInstanceMetadata ->
  -- | 'nsInstanceId'
  Prelude.Text ->
  -- | 'vnfPkgId'
  Prelude.Text ->
  -- | 'vnfdId'
  Prelude.Text ->
  GetSolFunctionInstanceResponse
newGetSolFunctionInstanceResponse
  pHttpStatus_
  pArn_
  pId_
  pInstantiationState_
  pMetadata_
  pNsInstanceId_
  pVnfPkgId_
  pVnfdId_ =
    GetSolFunctionInstanceResponse'
      { instantiatedVnfInfo =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        vnfProductName = Prelude.Nothing,
        vnfProvider = Prelude.Nothing,
        vnfdVersion = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        id = pId_,
        instantiationState = pInstantiationState_,
        metadata = pMetadata_,
        nsInstanceId = pNsInstanceId_,
        vnfPkgId = pVnfPkgId_,
        vnfdId = pVnfdId_
      }

-- | Undocumented member.
getSolFunctionInstanceResponse_instantiatedVnfInfo :: Lens.Lens' GetSolFunctionInstanceResponse (Prelude.Maybe GetSolVnfInfo)
getSolFunctionInstanceResponse_instantiatedVnfInfo = Lens.lens (\GetSolFunctionInstanceResponse' {instantiatedVnfInfo} -> instantiatedVnfInfo) (\s@GetSolFunctionInstanceResponse' {} a -> s {instantiatedVnfInfo = a} :: GetSolFunctionInstanceResponse)

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
getSolFunctionInstanceResponse_tags :: Lens.Lens' GetSolFunctionInstanceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getSolFunctionInstanceResponse_tags = Lens.lens (\GetSolFunctionInstanceResponse' {tags} -> tags) (\s@GetSolFunctionInstanceResponse' {} a -> s {tags = a} :: GetSolFunctionInstanceResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Network function product name.
getSolFunctionInstanceResponse_vnfProductName :: Lens.Lens' GetSolFunctionInstanceResponse (Prelude.Maybe Prelude.Text)
getSolFunctionInstanceResponse_vnfProductName = Lens.lens (\GetSolFunctionInstanceResponse' {vnfProductName} -> vnfProductName) (\s@GetSolFunctionInstanceResponse' {} a -> s {vnfProductName = a} :: GetSolFunctionInstanceResponse)

-- | Network function provider.
getSolFunctionInstanceResponse_vnfProvider :: Lens.Lens' GetSolFunctionInstanceResponse (Prelude.Maybe Prelude.Text)
getSolFunctionInstanceResponse_vnfProvider = Lens.lens (\GetSolFunctionInstanceResponse' {vnfProvider} -> vnfProvider) (\s@GetSolFunctionInstanceResponse' {} a -> s {vnfProvider = a} :: GetSolFunctionInstanceResponse)

-- | Function package descriptor version.
getSolFunctionInstanceResponse_vnfdVersion :: Lens.Lens' GetSolFunctionInstanceResponse (Prelude.Maybe Prelude.Text)
getSolFunctionInstanceResponse_vnfdVersion = Lens.lens (\GetSolFunctionInstanceResponse' {vnfdVersion} -> vnfdVersion) (\s@GetSolFunctionInstanceResponse' {} a -> s {vnfdVersion = a} :: GetSolFunctionInstanceResponse)

-- | The response's http status code.
getSolFunctionInstanceResponse_httpStatus :: Lens.Lens' GetSolFunctionInstanceResponse Prelude.Int
getSolFunctionInstanceResponse_httpStatus = Lens.lens (\GetSolFunctionInstanceResponse' {httpStatus} -> httpStatus) (\s@GetSolFunctionInstanceResponse' {} a -> s {httpStatus = a} :: GetSolFunctionInstanceResponse)

-- | Network function instance ARN.
getSolFunctionInstanceResponse_arn :: Lens.Lens' GetSolFunctionInstanceResponse Prelude.Text
getSolFunctionInstanceResponse_arn = Lens.lens (\GetSolFunctionInstanceResponse' {arn} -> arn) (\s@GetSolFunctionInstanceResponse' {} a -> s {arn = a} :: GetSolFunctionInstanceResponse)

-- | Network function instance ID.
getSolFunctionInstanceResponse_id :: Lens.Lens' GetSolFunctionInstanceResponse Prelude.Text
getSolFunctionInstanceResponse_id = Lens.lens (\GetSolFunctionInstanceResponse' {id} -> id) (\s@GetSolFunctionInstanceResponse' {} a -> s {id = a} :: GetSolFunctionInstanceResponse)

-- | Network function instantiation state.
getSolFunctionInstanceResponse_instantiationState :: Lens.Lens' GetSolFunctionInstanceResponse VnfInstantiationState
getSolFunctionInstanceResponse_instantiationState = Lens.lens (\GetSolFunctionInstanceResponse' {instantiationState} -> instantiationState) (\s@GetSolFunctionInstanceResponse' {} a -> s {instantiationState = a} :: GetSolFunctionInstanceResponse)

-- | Undocumented member.
getSolFunctionInstanceResponse_metadata :: Lens.Lens' GetSolFunctionInstanceResponse GetSolFunctionInstanceMetadata
getSolFunctionInstanceResponse_metadata = Lens.lens (\GetSolFunctionInstanceResponse' {metadata} -> metadata) (\s@GetSolFunctionInstanceResponse' {} a -> s {metadata = a} :: GetSolFunctionInstanceResponse)

-- | Network instance ID.
getSolFunctionInstanceResponse_nsInstanceId :: Lens.Lens' GetSolFunctionInstanceResponse Prelude.Text
getSolFunctionInstanceResponse_nsInstanceId = Lens.lens (\GetSolFunctionInstanceResponse' {nsInstanceId} -> nsInstanceId) (\s@GetSolFunctionInstanceResponse' {} a -> s {nsInstanceId = a} :: GetSolFunctionInstanceResponse)

-- | Function package ID.
getSolFunctionInstanceResponse_vnfPkgId :: Lens.Lens' GetSolFunctionInstanceResponse Prelude.Text
getSolFunctionInstanceResponse_vnfPkgId = Lens.lens (\GetSolFunctionInstanceResponse' {vnfPkgId} -> vnfPkgId) (\s@GetSolFunctionInstanceResponse' {} a -> s {vnfPkgId = a} :: GetSolFunctionInstanceResponse)

-- | Function package descriptor ID.
getSolFunctionInstanceResponse_vnfdId :: Lens.Lens' GetSolFunctionInstanceResponse Prelude.Text
getSolFunctionInstanceResponse_vnfdId = Lens.lens (\GetSolFunctionInstanceResponse' {vnfdId} -> vnfdId) (\s@GetSolFunctionInstanceResponse' {} a -> s {vnfdId = a} :: GetSolFunctionInstanceResponse)

instance
  Prelude.NFData
    GetSolFunctionInstanceResponse
  where
  rnf GetSolFunctionInstanceResponse' {..} =
    Prelude.rnf instantiatedVnfInfo
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vnfProductName
      `Prelude.seq` Prelude.rnf vnfProvider
      `Prelude.seq` Prelude.rnf vnfdVersion
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf instantiationState
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf nsInstanceId
      `Prelude.seq` Prelude.rnf vnfPkgId
      `Prelude.seq` Prelude.rnf vnfdId
