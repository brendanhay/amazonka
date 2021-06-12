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
-- Module      : Network.AWS.DirectoryService.RemoveIpRoutes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes IP address blocks from a directory.
module Network.AWS.DirectoryService.RemoveIpRoutes
  ( -- * Creating a Request
    RemoveIpRoutes (..),
    newRemoveIpRoutes,

    -- * Request Lenses
    removeIpRoutes_directoryId,
    removeIpRoutes_cidrIps,

    -- * Destructuring the Response
    RemoveIpRoutesResponse (..),
    newRemoveIpRoutesResponse,

    -- * Response Lenses
    removeIpRoutesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveIpRoutes' smart constructor.
data RemoveIpRoutes = RemoveIpRoutes'
  { -- | Identifier (ID) of the directory from which you want to remove the IP
    -- addresses.
    directoryId :: Core.Text,
    -- | IP address blocks that you want to remove.
    cidrIps :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveIpRoutes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'removeIpRoutes_directoryId' - Identifier (ID) of the directory from which you want to remove the IP
-- addresses.
--
-- 'cidrIps', 'removeIpRoutes_cidrIps' - IP address blocks that you want to remove.
newRemoveIpRoutes ::
  -- | 'directoryId'
  Core.Text ->
  RemoveIpRoutes
newRemoveIpRoutes pDirectoryId_ =
  RemoveIpRoutes'
    { directoryId = pDirectoryId_,
      cidrIps = Core.mempty
    }

-- | Identifier (ID) of the directory from which you want to remove the IP
-- addresses.
removeIpRoutes_directoryId :: Lens.Lens' RemoveIpRoutes Core.Text
removeIpRoutes_directoryId = Lens.lens (\RemoveIpRoutes' {directoryId} -> directoryId) (\s@RemoveIpRoutes' {} a -> s {directoryId = a} :: RemoveIpRoutes)

-- | IP address blocks that you want to remove.
removeIpRoutes_cidrIps :: Lens.Lens' RemoveIpRoutes [Core.Text]
removeIpRoutes_cidrIps = Lens.lens (\RemoveIpRoutes' {cidrIps} -> cidrIps) (\s@RemoveIpRoutes' {} a -> s {cidrIps = a} :: RemoveIpRoutes) Core.. Lens._Coerce

instance Core.AWSRequest RemoveIpRoutes where
  type
    AWSResponse RemoveIpRoutes =
      RemoveIpRoutesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveIpRoutesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RemoveIpRoutes

instance Core.NFData RemoveIpRoutes

instance Core.ToHeaders RemoveIpRoutes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.RemoveIpRoutes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RemoveIpRoutes where
  toJSON RemoveIpRoutes' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("CidrIps" Core..= cidrIps)
          ]
      )

instance Core.ToPath RemoveIpRoutes where
  toPath = Core.const "/"

instance Core.ToQuery RemoveIpRoutes where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRemoveIpRoutesResponse' smart constructor.
data RemoveIpRoutesResponse = RemoveIpRoutesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveIpRoutesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeIpRoutesResponse_httpStatus' - The response's http status code.
newRemoveIpRoutesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RemoveIpRoutesResponse
newRemoveIpRoutesResponse pHttpStatus_ =
  RemoveIpRoutesResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
removeIpRoutesResponse_httpStatus :: Lens.Lens' RemoveIpRoutesResponse Core.Int
removeIpRoutesResponse_httpStatus = Lens.lens (\RemoveIpRoutesResponse' {httpStatus} -> httpStatus) (\s@RemoveIpRoutesResponse' {} a -> s {httpStatus = a} :: RemoveIpRoutesResponse)

instance Core.NFData RemoveIpRoutesResponse
