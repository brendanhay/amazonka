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
-- Module      : Amazonka.StorageGateway.AddWorkingStorage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures one or more gateway local disks as working storage for a
-- gateway. This operation is only supported in the stored volume gateway
-- type. This operation is deprecated in cached volume API version
-- 20120630. Use AddUploadBuffer instead.
--
-- Working storage is also referred to as upload buffer. You can also use
-- the AddUploadBuffer operation to add upload buffer to a stored volume
-- gateway.
--
-- In the request, you specify the gateway Amazon Resource Name (ARN) to
-- which you want to add working storage, and one or more disk IDs that you
-- want to configure as working storage.
module Amazonka.StorageGateway.AddWorkingStorage
  ( -- * Creating a Request
    AddWorkingStorage (..),
    newAddWorkingStorage,

    -- * Request Lenses
    addWorkingStorage_gatewayARN,
    addWorkingStorage_diskIds,

    -- * Destructuring the Response
    AddWorkingStorageResponse (..),
    newAddWorkingStorageResponse,

    -- * Response Lenses
    addWorkingStorageResponse_gatewayARN,
    addWorkingStorageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
-- -   AddWorkingStorageInput$DiskIds
--
-- /See:/ 'newAddWorkingStorage' smart constructor.
data AddWorkingStorage = AddWorkingStorage'
  { gatewayARN :: Prelude.Text,
    -- | An array of strings that identify disks that are to be configured as
    -- working storage. Each string has a minimum length of 1 and maximum
    -- length of 300. You can get the disk IDs from the ListLocalDisks API.
    diskIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddWorkingStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'addWorkingStorage_gatewayARN' - Undocumented member.
--
-- 'diskIds', 'addWorkingStorage_diskIds' - An array of strings that identify disks that are to be configured as
-- working storage. Each string has a minimum length of 1 and maximum
-- length of 300. You can get the disk IDs from the ListLocalDisks API.
newAddWorkingStorage ::
  -- | 'gatewayARN'
  Prelude.Text ->
  AddWorkingStorage
newAddWorkingStorage pGatewayARN_ =
  AddWorkingStorage'
    { gatewayARN = pGatewayARN_,
      diskIds = Prelude.mempty
    }

-- | Undocumented member.
addWorkingStorage_gatewayARN :: Lens.Lens' AddWorkingStorage Prelude.Text
addWorkingStorage_gatewayARN = Lens.lens (\AddWorkingStorage' {gatewayARN} -> gatewayARN) (\s@AddWorkingStorage' {} a -> s {gatewayARN = a} :: AddWorkingStorage)

-- | An array of strings that identify disks that are to be configured as
-- working storage. Each string has a minimum length of 1 and maximum
-- length of 300. You can get the disk IDs from the ListLocalDisks API.
addWorkingStorage_diskIds :: Lens.Lens' AddWorkingStorage [Prelude.Text]
addWorkingStorage_diskIds = Lens.lens (\AddWorkingStorage' {diskIds} -> diskIds) (\s@AddWorkingStorage' {} a -> s {diskIds = a} :: AddWorkingStorage) Prelude.. Lens.coerced

instance Core.AWSRequest AddWorkingStorage where
  type
    AWSResponse AddWorkingStorage =
      AddWorkingStorageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddWorkingStorageResponse'
            Prelude.<$> (x Data..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddWorkingStorage where
  hashWithSalt _salt AddWorkingStorage' {..} =
    _salt
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` diskIds

instance Prelude.NFData AddWorkingStorage where
  rnf AddWorkingStorage' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf diskIds

instance Data.ToHeaders AddWorkingStorage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.AddWorkingStorage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddWorkingStorage where
  toJSON AddWorkingStorage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GatewayARN" Data..= gatewayARN),
            Prelude.Just ("DiskIds" Data..= diskIds)
          ]
      )

instance Data.ToPath AddWorkingStorage where
  toPath = Prelude.const "/"

instance Data.ToQuery AddWorkingStorage where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway
-- for which working storage was configured.
--
-- /See:/ 'newAddWorkingStorageResponse' smart constructor.
data AddWorkingStorageResponse = AddWorkingStorageResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddWorkingStorageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'addWorkingStorageResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'addWorkingStorageResponse_httpStatus' - The response's http status code.
newAddWorkingStorageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddWorkingStorageResponse
newAddWorkingStorageResponse pHttpStatus_ =
  AddWorkingStorageResponse'
    { gatewayARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
addWorkingStorageResponse_gatewayARN :: Lens.Lens' AddWorkingStorageResponse (Prelude.Maybe Prelude.Text)
addWorkingStorageResponse_gatewayARN = Lens.lens (\AddWorkingStorageResponse' {gatewayARN} -> gatewayARN) (\s@AddWorkingStorageResponse' {} a -> s {gatewayARN = a} :: AddWorkingStorageResponse)

-- | The response's http status code.
addWorkingStorageResponse_httpStatus :: Lens.Lens' AddWorkingStorageResponse Prelude.Int
addWorkingStorageResponse_httpStatus = Lens.lens (\AddWorkingStorageResponse' {httpStatus} -> httpStatus) (\s@AddWorkingStorageResponse' {} a -> s {httpStatus = a} :: AddWorkingStorageResponse)

instance Prelude.NFData AddWorkingStorageResponse where
  rnf AddWorkingStorageResponse' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf httpStatus
