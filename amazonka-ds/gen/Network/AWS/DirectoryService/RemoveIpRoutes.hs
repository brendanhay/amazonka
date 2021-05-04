{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveIpRoutes' smart constructor.
data RemoveIpRoutes = RemoveIpRoutes'
  { -- | Identifier (ID) of the directory from which you want to remove the IP
    -- addresses.
    directoryId :: Prelude.Text,
    -- | IP address blocks that you want to remove.
    cidrIps :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  RemoveIpRoutes
newRemoveIpRoutes pDirectoryId_ =
  RemoveIpRoutes'
    { directoryId = pDirectoryId_,
      cidrIps = Prelude.mempty
    }

-- | Identifier (ID) of the directory from which you want to remove the IP
-- addresses.
removeIpRoutes_directoryId :: Lens.Lens' RemoveIpRoutes Prelude.Text
removeIpRoutes_directoryId = Lens.lens (\RemoveIpRoutes' {directoryId} -> directoryId) (\s@RemoveIpRoutes' {} a -> s {directoryId = a} :: RemoveIpRoutes)

-- | IP address blocks that you want to remove.
removeIpRoutes_cidrIps :: Lens.Lens' RemoveIpRoutes [Prelude.Text]
removeIpRoutes_cidrIps = Lens.lens (\RemoveIpRoutes' {cidrIps} -> cidrIps) (\s@RemoveIpRoutes' {} a -> s {cidrIps = a} :: RemoveIpRoutes) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest RemoveIpRoutes where
  type Rs RemoveIpRoutes = RemoveIpRoutesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveIpRoutesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveIpRoutes

instance Prelude.NFData RemoveIpRoutes

instance Prelude.ToHeaders RemoveIpRoutes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.RemoveIpRoutes" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RemoveIpRoutes where
  toJSON RemoveIpRoutes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Prelude..= directoryId),
            Prelude.Just ("CidrIps" Prelude..= cidrIps)
          ]
      )

instance Prelude.ToPath RemoveIpRoutes where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RemoveIpRoutes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveIpRoutesResponse' smart constructor.
data RemoveIpRoutesResponse = RemoveIpRoutesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  RemoveIpRoutesResponse
newRemoveIpRoutesResponse pHttpStatus_ =
  RemoveIpRoutesResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
removeIpRoutesResponse_httpStatus :: Lens.Lens' RemoveIpRoutesResponse Prelude.Int
removeIpRoutesResponse_httpStatus = Lens.lens (\RemoveIpRoutesResponse' {httpStatus} -> httpStatus) (\s@RemoveIpRoutesResponse' {} a -> s {httpStatus = a} :: RemoveIpRoutesResponse)

instance Prelude.NFData RemoveIpRoutesResponse
