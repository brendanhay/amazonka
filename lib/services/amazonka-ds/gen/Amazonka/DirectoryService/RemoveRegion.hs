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
-- Module      : Amazonka.DirectoryService.RemoveRegion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops all replication and removes the domain controllers from the
-- specified Region. You cannot remove the primary Region with this
-- operation. Instead, use the @DeleteDirectory@ API.
module Amazonka.DirectoryService.RemoveRegion
  ( -- * Creating a Request
    RemoveRegion (..),
    newRemoveRegion,

    -- * Request Lenses
    removeRegion_directoryId,

    -- * Destructuring the Response
    RemoveRegionResponse (..),
    newRemoveRegionResponse,

    -- * Response Lenses
    removeRegionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveRegion' smart constructor.
data RemoveRegion = RemoveRegion'
  { -- | The identifier of the directory for which you want to remove Region
    -- replication.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveRegion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'removeRegion_directoryId' - The identifier of the directory for which you want to remove Region
-- replication.
newRemoveRegion ::
  -- | 'directoryId'
  Prelude.Text ->
  RemoveRegion
newRemoveRegion pDirectoryId_ =
  RemoveRegion' {directoryId = pDirectoryId_}

-- | The identifier of the directory for which you want to remove Region
-- replication.
removeRegion_directoryId :: Lens.Lens' RemoveRegion Prelude.Text
removeRegion_directoryId = Lens.lens (\RemoveRegion' {directoryId} -> directoryId) (\s@RemoveRegion' {} a -> s {directoryId = a} :: RemoveRegion)

instance Core.AWSRequest RemoveRegion where
  type AWSResponse RemoveRegion = RemoveRegionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveRegionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveRegion where
  hashWithSalt _salt RemoveRegion' {..} =
    _salt `Prelude.hashWithSalt` directoryId

instance Prelude.NFData RemoveRegion where
  rnf RemoveRegion' {..} = Prelude.rnf directoryId

instance Core.ToHeaders RemoveRegion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.RemoveRegion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RemoveRegion where
  toJSON RemoveRegion' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("DirectoryId" Core..= directoryId)]
      )

instance Core.ToPath RemoveRegion where
  toPath = Prelude.const "/"

instance Core.ToQuery RemoveRegion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveRegionResponse' smart constructor.
data RemoveRegionResponse = RemoveRegionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveRegionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeRegionResponse_httpStatus' - The response's http status code.
newRemoveRegionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveRegionResponse
newRemoveRegionResponse pHttpStatus_ =
  RemoveRegionResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
removeRegionResponse_httpStatus :: Lens.Lens' RemoveRegionResponse Prelude.Int
removeRegionResponse_httpStatus = Lens.lens (\RemoveRegionResponse' {httpStatus} -> httpStatus) (\s@RemoveRegionResponse' {} a -> s {httpStatus = a} :: RemoveRegionResponse)

instance Prelude.NFData RemoveRegionResponse where
  rnf RemoveRegionResponse' {..} =
    Prelude.rnf httpStatus
