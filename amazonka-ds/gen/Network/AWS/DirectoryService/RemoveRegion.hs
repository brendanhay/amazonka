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
-- Module      : Network.AWS.DirectoryService.RemoveRegion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops all replication and removes the domain controllers from the
-- specified Region. You cannot remove the primary Region with this
-- operation. Instead, use the @DeleteDirectory@ API.
module Network.AWS.DirectoryService.RemoveRegion
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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveRegion' smart constructor.
data RemoveRegion = RemoveRegion'
  { -- | The identifier of the directory for which you want to remove Region
    -- replication.
    directoryId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  RemoveRegion
newRemoveRegion pDirectoryId_ =
  RemoveRegion' {directoryId = pDirectoryId_}

-- | The identifier of the directory for which you want to remove Region
-- replication.
removeRegion_directoryId :: Lens.Lens' RemoveRegion Core.Text
removeRegion_directoryId = Lens.lens (\RemoveRegion' {directoryId} -> directoryId) (\s@RemoveRegion' {} a -> s {directoryId = a} :: RemoveRegion)

instance Core.AWSRequest RemoveRegion where
  type AWSResponse RemoveRegion = RemoveRegionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveRegionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RemoveRegion

instance Core.NFData RemoveRegion

instance Core.ToHeaders RemoveRegion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.RemoveRegion" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RemoveRegion where
  toJSON RemoveRegion' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("DirectoryId" Core..= directoryId)]
      )

instance Core.ToPath RemoveRegion where
  toPath = Core.const "/"

instance Core.ToQuery RemoveRegion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRemoveRegionResponse' smart constructor.
data RemoveRegionResponse = RemoveRegionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  RemoveRegionResponse
newRemoveRegionResponse pHttpStatus_ =
  RemoveRegionResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
removeRegionResponse_httpStatus :: Lens.Lens' RemoveRegionResponse Core.Int
removeRegionResponse_httpStatus = Lens.lens (\RemoveRegionResponse' {httpStatus} -> httpStatus) (\s@RemoveRegionResponse' {} a -> s {httpStatus = a} :: RemoveRegionResponse)

instance Core.NFData RemoveRegionResponse
