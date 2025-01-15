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
-- Module      : Amazonka.DirectoryService.RemoveIpRoutes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes IP address blocks from a directory.
module Amazonka.DirectoryService.RemoveIpRoutes
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveIpRoutes' smart constructor.
data RemoveIpRoutes = RemoveIpRoutes'
  { -- | Identifier (ID) of the directory from which you want to remove the IP
    -- addresses.
    directoryId :: Prelude.Text,
    -- | IP address blocks that you want to remove.
    cidrIps :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
removeIpRoutes_cidrIps = Lens.lens (\RemoveIpRoutes' {cidrIps} -> cidrIps) (\s@RemoveIpRoutes' {} a -> s {cidrIps = a} :: RemoveIpRoutes) Prelude.. Lens.coerced

instance Core.AWSRequest RemoveIpRoutes where
  type
    AWSResponse RemoveIpRoutes =
      RemoveIpRoutesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveIpRoutesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveIpRoutes where
  hashWithSalt _salt RemoveIpRoutes' {..} =
    _salt
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` cidrIps

instance Prelude.NFData RemoveIpRoutes where
  rnf RemoveIpRoutes' {..} =
    Prelude.rnf directoryId `Prelude.seq`
      Prelude.rnf cidrIps

instance Data.ToHeaders RemoveIpRoutes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.RemoveIpRoutes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemoveIpRoutes where
  toJSON RemoveIpRoutes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just ("CidrIps" Data..= cidrIps)
          ]
      )

instance Data.ToPath RemoveIpRoutes where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveIpRoutes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveIpRoutesResponse' smart constructor.
data RemoveIpRoutesResponse = RemoveIpRoutesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData RemoveIpRoutesResponse where
  rnf RemoveIpRoutesResponse' {..} =
    Prelude.rnf httpStatus
