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
-- Module      : Amazonka.TNB.GetSolNetworkInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of the network instance.
--
-- A network instance is a single network created in Amazon Web Services
-- TNB that can be deployed and on which life-cycle operations (like
-- terminate, update, and delete) can be performed.
module Amazonka.TNB.GetSolNetworkInstance
  ( -- * Creating a Request
    GetSolNetworkInstance (..),
    newGetSolNetworkInstance,

    -- * Request Lenses
    getSolNetworkInstance_nsInstanceId,

    -- * Destructuring the Response
    GetSolNetworkInstanceResponse (..),
    newGetSolNetworkInstanceResponse,

    -- * Response Lenses
    getSolNetworkInstanceResponse_lcmOpInfo,
    getSolNetworkInstanceResponse_nsState,
    getSolNetworkInstanceResponse_tags,
    getSolNetworkInstanceResponse_httpStatus,
    getSolNetworkInstanceResponse_arn,
    getSolNetworkInstanceResponse_id,
    getSolNetworkInstanceResponse_metadata,
    getSolNetworkInstanceResponse_nsInstanceDescription,
    getSolNetworkInstanceResponse_nsInstanceName,
    getSolNetworkInstanceResponse_nsdId,
    getSolNetworkInstanceResponse_nsdInfoId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newGetSolNetworkInstance' smart constructor.
data GetSolNetworkInstance = GetSolNetworkInstance'
  { -- | ID of the network instance.
    nsInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolNetworkInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nsInstanceId', 'getSolNetworkInstance_nsInstanceId' - ID of the network instance.
newGetSolNetworkInstance ::
  -- | 'nsInstanceId'
  Prelude.Text ->
  GetSolNetworkInstance
newGetSolNetworkInstance pNsInstanceId_ =
  GetSolNetworkInstance'
    { nsInstanceId =
        pNsInstanceId_
    }

-- | ID of the network instance.
getSolNetworkInstance_nsInstanceId :: Lens.Lens' GetSolNetworkInstance Prelude.Text
getSolNetworkInstance_nsInstanceId = Lens.lens (\GetSolNetworkInstance' {nsInstanceId} -> nsInstanceId) (\s@GetSolNetworkInstance' {} a -> s {nsInstanceId = a} :: GetSolNetworkInstance)

instance Core.AWSRequest GetSolNetworkInstance where
  type
    AWSResponse GetSolNetworkInstance =
      GetSolNetworkInstanceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSolNetworkInstanceResponse'
            Prelude.<$> (x Data..?> "lcmOpInfo")
            Prelude.<*> (x Data..?> "nsState")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "metadata")
            Prelude.<*> (x Data..:> "nsInstanceDescription")
            Prelude.<*> (x Data..:> "nsInstanceName")
            Prelude.<*> (x Data..:> "nsdId")
            Prelude.<*> (x Data..:> "nsdInfoId")
      )

instance Prelude.Hashable GetSolNetworkInstance where
  hashWithSalt _salt GetSolNetworkInstance' {..} =
    _salt `Prelude.hashWithSalt` nsInstanceId

instance Prelude.NFData GetSolNetworkInstance where
  rnf GetSolNetworkInstance' {..} =
    Prelude.rnf nsInstanceId

instance Data.ToHeaders GetSolNetworkInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSolNetworkInstance where
  toPath GetSolNetworkInstance' {..} =
    Prelude.mconcat
      [ "/sol/nslcm/v1/ns_instances/",
        Data.toBS nsInstanceId
      ]

instance Data.ToQuery GetSolNetworkInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSolNetworkInstanceResponse' smart constructor.
data GetSolNetworkInstanceResponse = GetSolNetworkInstanceResponse'
  { lcmOpInfo :: Prelude.Maybe LcmOperationInfo,
    -- | Network instance state.
    nsState :: Prelude.Maybe NsState,
    -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. You can use tags to
    -- search and filter your resources or track your Amazon Web Services
    -- costs.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Network instance ARN.
    arn :: Prelude.Text,
    -- | Network instance ID.
    id :: Prelude.Text,
    metadata :: GetSolNetworkInstanceMetadata,
    -- | Network instance description.
    nsInstanceDescription :: Prelude.Text,
    -- | Network instance name.
    nsInstanceName :: Prelude.Text,
    -- | Network service descriptor ID.
    nsdId :: Prelude.Text,
    -- | Network service descriptor info ID.
    nsdInfoId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolNetworkInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lcmOpInfo', 'getSolNetworkInstanceResponse_lcmOpInfo' - Undocumented member.
--
-- 'nsState', 'getSolNetworkInstanceResponse_nsState' - Network instance state.
--
-- 'tags', 'getSolNetworkInstanceResponse_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
--
-- 'httpStatus', 'getSolNetworkInstanceResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getSolNetworkInstanceResponse_arn' - Network instance ARN.
--
-- 'id', 'getSolNetworkInstanceResponse_id' - Network instance ID.
--
-- 'metadata', 'getSolNetworkInstanceResponse_metadata' - Undocumented member.
--
-- 'nsInstanceDescription', 'getSolNetworkInstanceResponse_nsInstanceDescription' - Network instance description.
--
-- 'nsInstanceName', 'getSolNetworkInstanceResponse_nsInstanceName' - Network instance name.
--
-- 'nsdId', 'getSolNetworkInstanceResponse_nsdId' - Network service descriptor ID.
--
-- 'nsdInfoId', 'getSolNetworkInstanceResponse_nsdInfoId' - Network service descriptor info ID.
newGetSolNetworkInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'metadata'
  GetSolNetworkInstanceMetadata ->
  -- | 'nsInstanceDescription'
  Prelude.Text ->
  -- | 'nsInstanceName'
  Prelude.Text ->
  -- | 'nsdId'
  Prelude.Text ->
  -- | 'nsdInfoId'
  Prelude.Text ->
  GetSolNetworkInstanceResponse
newGetSolNetworkInstanceResponse
  pHttpStatus_
  pArn_
  pId_
  pMetadata_
  pNsInstanceDescription_
  pNsInstanceName_
  pNsdId_
  pNsdInfoId_ =
    GetSolNetworkInstanceResponse'
      { lcmOpInfo =
          Prelude.Nothing,
        nsState = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        id = pId_,
        metadata = pMetadata_,
        nsInstanceDescription =
          pNsInstanceDescription_,
        nsInstanceName = pNsInstanceName_,
        nsdId = pNsdId_,
        nsdInfoId = pNsdInfoId_
      }

-- | Undocumented member.
getSolNetworkInstanceResponse_lcmOpInfo :: Lens.Lens' GetSolNetworkInstanceResponse (Prelude.Maybe LcmOperationInfo)
getSolNetworkInstanceResponse_lcmOpInfo = Lens.lens (\GetSolNetworkInstanceResponse' {lcmOpInfo} -> lcmOpInfo) (\s@GetSolNetworkInstanceResponse' {} a -> s {lcmOpInfo = a} :: GetSolNetworkInstanceResponse)

-- | Network instance state.
getSolNetworkInstanceResponse_nsState :: Lens.Lens' GetSolNetworkInstanceResponse (Prelude.Maybe NsState)
getSolNetworkInstanceResponse_nsState = Lens.lens (\GetSolNetworkInstanceResponse' {nsState} -> nsState) (\s@GetSolNetworkInstanceResponse' {} a -> s {nsState = a} :: GetSolNetworkInstanceResponse)

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
getSolNetworkInstanceResponse_tags :: Lens.Lens' GetSolNetworkInstanceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getSolNetworkInstanceResponse_tags = Lens.lens (\GetSolNetworkInstanceResponse' {tags} -> tags) (\s@GetSolNetworkInstanceResponse' {} a -> s {tags = a} :: GetSolNetworkInstanceResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
getSolNetworkInstanceResponse_httpStatus :: Lens.Lens' GetSolNetworkInstanceResponse Prelude.Int
getSolNetworkInstanceResponse_httpStatus = Lens.lens (\GetSolNetworkInstanceResponse' {httpStatus} -> httpStatus) (\s@GetSolNetworkInstanceResponse' {} a -> s {httpStatus = a} :: GetSolNetworkInstanceResponse)

-- | Network instance ARN.
getSolNetworkInstanceResponse_arn :: Lens.Lens' GetSolNetworkInstanceResponse Prelude.Text
getSolNetworkInstanceResponse_arn = Lens.lens (\GetSolNetworkInstanceResponse' {arn} -> arn) (\s@GetSolNetworkInstanceResponse' {} a -> s {arn = a} :: GetSolNetworkInstanceResponse)

-- | Network instance ID.
getSolNetworkInstanceResponse_id :: Lens.Lens' GetSolNetworkInstanceResponse Prelude.Text
getSolNetworkInstanceResponse_id = Lens.lens (\GetSolNetworkInstanceResponse' {id} -> id) (\s@GetSolNetworkInstanceResponse' {} a -> s {id = a} :: GetSolNetworkInstanceResponse)

-- | Undocumented member.
getSolNetworkInstanceResponse_metadata :: Lens.Lens' GetSolNetworkInstanceResponse GetSolNetworkInstanceMetadata
getSolNetworkInstanceResponse_metadata = Lens.lens (\GetSolNetworkInstanceResponse' {metadata} -> metadata) (\s@GetSolNetworkInstanceResponse' {} a -> s {metadata = a} :: GetSolNetworkInstanceResponse)

-- | Network instance description.
getSolNetworkInstanceResponse_nsInstanceDescription :: Lens.Lens' GetSolNetworkInstanceResponse Prelude.Text
getSolNetworkInstanceResponse_nsInstanceDescription = Lens.lens (\GetSolNetworkInstanceResponse' {nsInstanceDescription} -> nsInstanceDescription) (\s@GetSolNetworkInstanceResponse' {} a -> s {nsInstanceDescription = a} :: GetSolNetworkInstanceResponse)

-- | Network instance name.
getSolNetworkInstanceResponse_nsInstanceName :: Lens.Lens' GetSolNetworkInstanceResponse Prelude.Text
getSolNetworkInstanceResponse_nsInstanceName = Lens.lens (\GetSolNetworkInstanceResponse' {nsInstanceName} -> nsInstanceName) (\s@GetSolNetworkInstanceResponse' {} a -> s {nsInstanceName = a} :: GetSolNetworkInstanceResponse)

-- | Network service descriptor ID.
getSolNetworkInstanceResponse_nsdId :: Lens.Lens' GetSolNetworkInstanceResponse Prelude.Text
getSolNetworkInstanceResponse_nsdId = Lens.lens (\GetSolNetworkInstanceResponse' {nsdId} -> nsdId) (\s@GetSolNetworkInstanceResponse' {} a -> s {nsdId = a} :: GetSolNetworkInstanceResponse)

-- | Network service descriptor info ID.
getSolNetworkInstanceResponse_nsdInfoId :: Lens.Lens' GetSolNetworkInstanceResponse Prelude.Text
getSolNetworkInstanceResponse_nsdInfoId = Lens.lens (\GetSolNetworkInstanceResponse' {nsdInfoId} -> nsdInfoId) (\s@GetSolNetworkInstanceResponse' {} a -> s {nsdInfoId = a} :: GetSolNetworkInstanceResponse)

instance Prelude.NFData GetSolNetworkInstanceResponse where
  rnf GetSolNetworkInstanceResponse' {..} =
    Prelude.rnf lcmOpInfo
      `Prelude.seq` Prelude.rnf nsState
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf nsInstanceDescription
      `Prelude.seq` Prelude.rnf nsInstanceName
      `Prelude.seq` Prelude.rnf nsdId
      `Prelude.seq` Prelude.rnf nsdInfoId
