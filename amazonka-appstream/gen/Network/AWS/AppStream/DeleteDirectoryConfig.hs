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
-- Module      : Network.AWS.AppStream.DeleteDirectoryConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Directory Config object from AppStream 2.0. This
-- object includes the information required to join streaming instances to
-- an Active Directory domain.
module Network.AWS.AppStream.DeleteDirectoryConfig
  ( -- * Creating a Request
    DeleteDirectoryConfig (..),
    newDeleteDirectoryConfig,

    -- * Request Lenses
    deleteDirectoryConfig_directoryName,

    -- * Destructuring the Response
    DeleteDirectoryConfigResponse (..),
    newDeleteDirectoryConfigResponse,

    -- * Response Lenses
    deleteDirectoryConfigResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDirectoryConfig' smart constructor.
data DeleteDirectoryConfig = DeleteDirectoryConfig'
  { -- | The name of the directory configuration.
    directoryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDirectoryConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryName', 'deleteDirectoryConfig_directoryName' - The name of the directory configuration.
newDeleteDirectoryConfig ::
  -- | 'directoryName'
  Core.Text ->
  DeleteDirectoryConfig
newDeleteDirectoryConfig pDirectoryName_ =
  DeleteDirectoryConfig'
    { directoryName =
        pDirectoryName_
    }

-- | The name of the directory configuration.
deleteDirectoryConfig_directoryName :: Lens.Lens' DeleteDirectoryConfig Core.Text
deleteDirectoryConfig_directoryName = Lens.lens (\DeleteDirectoryConfig' {directoryName} -> directoryName) (\s@DeleteDirectoryConfig' {} a -> s {directoryName = a} :: DeleteDirectoryConfig)

instance Core.AWSRequest DeleteDirectoryConfig where
  type
    AWSResponse DeleteDirectoryConfig =
      DeleteDirectoryConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDirectoryConfigResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteDirectoryConfig

instance Core.NFData DeleteDirectoryConfig

instance Core.ToHeaders DeleteDirectoryConfig where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DeleteDirectoryConfig" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteDirectoryConfig where
  toJSON DeleteDirectoryConfig' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("DirectoryName" Core..= directoryName)]
      )

instance Core.ToPath DeleteDirectoryConfig where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDirectoryConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteDirectoryConfigResponse' smart constructor.
data DeleteDirectoryConfigResponse = DeleteDirectoryConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDirectoryConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDirectoryConfigResponse_httpStatus' - The response's http status code.
newDeleteDirectoryConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteDirectoryConfigResponse
newDeleteDirectoryConfigResponse pHttpStatus_ =
  DeleteDirectoryConfigResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDirectoryConfigResponse_httpStatus :: Lens.Lens' DeleteDirectoryConfigResponse Core.Int
deleteDirectoryConfigResponse_httpStatus = Lens.lens (\DeleteDirectoryConfigResponse' {httpStatus} -> httpStatus) (\s@DeleteDirectoryConfigResponse' {} a -> s {httpStatus = a} :: DeleteDirectoryConfigResponse)

instance Core.NFData DeleteDirectoryConfigResponse
