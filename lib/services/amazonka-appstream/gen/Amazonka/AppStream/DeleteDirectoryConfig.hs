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
-- Module      : Amazonka.AppStream.DeleteDirectoryConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Directory Config object from AppStream 2.0. This
-- object includes the information required to join streaming instances to
-- an Active Directory domain.
module Amazonka.AppStream.DeleteDirectoryConfig
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

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDirectoryConfig' smart constructor.
data DeleteDirectoryConfig = DeleteDirectoryConfig'
  { -- | The name of the directory configuration.
    directoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteDirectoryConfig
newDeleteDirectoryConfig pDirectoryName_ =
  DeleteDirectoryConfig'
    { directoryName =
        pDirectoryName_
    }

-- | The name of the directory configuration.
deleteDirectoryConfig_directoryName :: Lens.Lens' DeleteDirectoryConfig Prelude.Text
deleteDirectoryConfig_directoryName = Lens.lens (\DeleteDirectoryConfig' {directoryName} -> directoryName) (\s@DeleteDirectoryConfig' {} a -> s {directoryName = a} :: DeleteDirectoryConfig)

instance Core.AWSRequest DeleteDirectoryConfig where
  type
    AWSResponse DeleteDirectoryConfig =
      DeleteDirectoryConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDirectoryConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDirectoryConfig where
  hashWithSalt _salt DeleteDirectoryConfig' {..} =
    _salt `Prelude.hashWithSalt` directoryName

instance Prelude.NFData DeleteDirectoryConfig where
  rnf DeleteDirectoryConfig' {..} =
    Prelude.rnf directoryName

instance Data.ToHeaders DeleteDirectoryConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DeleteDirectoryConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDirectoryConfig where
  toJSON DeleteDirectoryConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DirectoryName" Data..= directoryName)
          ]
      )

instance Data.ToPath DeleteDirectoryConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDirectoryConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDirectoryConfigResponse' smart constructor.
data DeleteDirectoryConfigResponse = DeleteDirectoryConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteDirectoryConfigResponse
newDeleteDirectoryConfigResponse pHttpStatus_ =
  DeleteDirectoryConfigResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDirectoryConfigResponse_httpStatus :: Lens.Lens' DeleteDirectoryConfigResponse Prelude.Int
deleteDirectoryConfigResponse_httpStatus = Lens.lens (\DeleteDirectoryConfigResponse' {httpStatus} -> httpStatus) (\s@DeleteDirectoryConfigResponse' {} a -> s {httpStatus = a} :: DeleteDirectoryConfigResponse)

instance Prelude.NFData DeleteDirectoryConfigResponse where
  rnf DeleteDirectoryConfigResponse' {..} =
    Prelude.rnf httpStatus
