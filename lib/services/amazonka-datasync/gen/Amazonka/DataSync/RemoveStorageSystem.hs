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
-- Module      : Amazonka.DataSync.RemoveStorageSystem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently removes a storage system resource from DataSync Discovery,
-- including the associated discovery jobs, collected data, and
-- recommendations.
module Amazonka.DataSync.RemoveStorageSystem
  ( -- * Creating a Request
    RemoveStorageSystem (..),
    newRemoveStorageSystem,

    -- * Request Lenses
    removeStorageSystem_storageSystemArn,

    -- * Destructuring the Response
    RemoveStorageSystemResponse (..),
    newRemoveStorageSystemResponse,

    -- * Response Lenses
    removeStorageSystemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveStorageSystem' smart constructor.
data RemoveStorageSystem = RemoveStorageSystem'
  { -- | Specifies the Amazon Resource Name (ARN) of the storage system that you
    -- want to permanently remove from DataSync Discovery.
    storageSystemArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveStorageSystem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageSystemArn', 'removeStorageSystem_storageSystemArn' - Specifies the Amazon Resource Name (ARN) of the storage system that you
-- want to permanently remove from DataSync Discovery.
newRemoveStorageSystem ::
  -- | 'storageSystemArn'
  Prelude.Text ->
  RemoveStorageSystem
newRemoveStorageSystem pStorageSystemArn_ =
  RemoveStorageSystem'
    { storageSystemArn =
        pStorageSystemArn_
    }

-- | Specifies the Amazon Resource Name (ARN) of the storage system that you
-- want to permanently remove from DataSync Discovery.
removeStorageSystem_storageSystemArn :: Lens.Lens' RemoveStorageSystem Prelude.Text
removeStorageSystem_storageSystemArn = Lens.lens (\RemoveStorageSystem' {storageSystemArn} -> storageSystemArn) (\s@RemoveStorageSystem' {} a -> s {storageSystemArn = a} :: RemoveStorageSystem)

instance Core.AWSRequest RemoveStorageSystem where
  type
    AWSResponse RemoveStorageSystem =
      RemoveStorageSystemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveStorageSystemResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveStorageSystem where
  hashWithSalt _salt RemoveStorageSystem' {..} =
    _salt `Prelude.hashWithSalt` storageSystemArn

instance Prelude.NFData RemoveStorageSystem where
  rnf RemoveStorageSystem' {..} =
    Prelude.rnf storageSystemArn

instance Data.ToHeaders RemoveStorageSystem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.RemoveStorageSystem" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemoveStorageSystem where
  toJSON RemoveStorageSystem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("StorageSystemArn" Data..= storageSystemArn)
          ]
      )

instance Data.ToPath RemoveStorageSystem where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveStorageSystem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveStorageSystemResponse' smart constructor.
data RemoveStorageSystemResponse = RemoveStorageSystemResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveStorageSystemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeStorageSystemResponse_httpStatus' - The response's http status code.
newRemoveStorageSystemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveStorageSystemResponse
newRemoveStorageSystemResponse pHttpStatus_ =
  RemoveStorageSystemResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
removeStorageSystemResponse_httpStatus :: Lens.Lens' RemoveStorageSystemResponse Prelude.Int
removeStorageSystemResponse_httpStatus = Lens.lens (\RemoveStorageSystemResponse' {httpStatus} -> httpStatus) (\s@RemoveStorageSystemResponse' {} a -> s {httpStatus = a} :: RemoveStorageSystemResponse)

instance Prelude.NFData RemoveStorageSystemResponse where
  rnf RemoveStorageSystemResponse' {..} =
    Prelude.rnf httpStatus
