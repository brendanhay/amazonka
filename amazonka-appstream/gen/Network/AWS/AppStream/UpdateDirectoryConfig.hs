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
-- Module      : Network.AWS.AppStream.UpdateDirectoryConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified Directory Config object in AppStream 2.0. This
-- object includes the configuration information required to join fleets
-- and image builders to Microsoft Active Directory domains.
module Network.AWS.AppStream.UpdateDirectoryConfig
  ( -- * Creating a Request
    UpdateDirectoryConfig (..),
    newUpdateDirectoryConfig,

    -- * Request Lenses
    updateDirectoryConfig_serviceAccountCredentials,
    updateDirectoryConfig_organizationalUnitDistinguishedNames,
    updateDirectoryConfig_directoryName,

    -- * Destructuring the Response
    UpdateDirectoryConfigResponse (..),
    newUpdateDirectoryConfigResponse,

    -- * Response Lenses
    updateDirectoryConfigResponse_directoryConfig,
    updateDirectoryConfigResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDirectoryConfig' smart constructor.
data UpdateDirectoryConfig = UpdateDirectoryConfig'
  { -- | The credentials for the service account used by the fleet or image
    -- builder to connect to the directory.
    serviceAccountCredentials :: Prelude.Maybe ServiceAccountCredentials,
    -- | The distinguished names of the organizational units for computer
    -- accounts.
    organizationalUnitDistinguishedNames :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Directory Config object.
    directoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDirectoryConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceAccountCredentials', 'updateDirectoryConfig_serviceAccountCredentials' - The credentials for the service account used by the fleet or image
-- builder to connect to the directory.
--
-- 'organizationalUnitDistinguishedNames', 'updateDirectoryConfig_organizationalUnitDistinguishedNames' - The distinguished names of the organizational units for computer
-- accounts.
--
-- 'directoryName', 'updateDirectoryConfig_directoryName' - The name of the Directory Config object.
newUpdateDirectoryConfig ::
  -- | 'directoryName'
  Prelude.Text ->
  UpdateDirectoryConfig
newUpdateDirectoryConfig pDirectoryName_ =
  UpdateDirectoryConfig'
    { serviceAccountCredentials =
        Prelude.Nothing,
      organizationalUnitDistinguishedNames =
        Prelude.Nothing,
      directoryName = pDirectoryName_
    }

-- | The credentials for the service account used by the fleet or image
-- builder to connect to the directory.
updateDirectoryConfig_serviceAccountCredentials :: Lens.Lens' UpdateDirectoryConfig (Prelude.Maybe ServiceAccountCredentials)
updateDirectoryConfig_serviceAccountCredentials = Lens.lens (\UpdateDirectoryConfig' {serviceAccountCredentials} -> serviceAccountCredentials) (\s@UpdateDirectoryConfig' {} a -> s {serviceAccountCredentials = a} :: UpdateDirectoryConfig)

-- | The distinguished names of the organizational units for computer
-- accounts.
updateDirectoryConfig_organizationalUnitDistinguishedNames :: Lens.Lens' UpdateDirectoryConfig (Prelude.Maybe [Prelude.Text])
updateDirectoryConfig_organizationalUnitDistinguishedNames = Lens.lens (\UpdateDirectoryConfig' {organizationalUnitDistinguishedNames} -> organizationalUnitDistinguishedNames) (\s@UpdateDirectoryConfig' {} a -> s {organizationalUnitDistinguishedNames = a} :: UpdateDirectoryConfig) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the Directory Config object.
updateDirectoryConfig_directoryName :: Lens.Lens' UpdateDirectoryConfig Prelude.Text
updateDirectoryConfig_directoryName = Lens.lens (\UpdateDirectoryConfig' {directoryName} -> directoryName) (\s@UpdateDirectoryConfig' {} a -> s {directoryName = a} :: UpdateDirectoryConfig)

instance Core.AWSRequest UpdateDirectoryConfig where
  type
    AWSResponse UpdateDirectoryConfig =
      UpdateDirectoryConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDirectoryConfigResponse'
            Prelude.<$> (x Core..?> "DirectoryConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDirectoryConfig

instance Prelude.NFData UpdateDirectoryConfig

instance Core.ToHeaders UpdateDirectoryConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.UpdateDirectoryConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDirectoryConfig where
  toJSON UpdateDirectoryConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ServiceAccountCredentials" Core..=)
              Prelude.<$> serviceAccountCredentials,
            ("OrganizationalUnitDistinguishedNames" Core..=)
              Prelude.<$> organizationalUnitDistinguishedNames,
            Prelude.Just
              ("DirectoryName" Core..= directoryName)
          ]
      )

instance Core.ToPath UpdateDirectoryConfig where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateDirectoryConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDirectoryConfigResponse' smart constructor.
data UpdateDirectoryConfigResponse = UpdateDirectoryConfigResponse'
  { -- | Information about the Directory Config object.
    directoryConfig :: Prelude.Maybe DirectoryConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDirectoryConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryConfig', 'updateDirectoryConfigResponse_directoryConfig' - Information about the Directory Config object.
--
-- 'httpStatus', 'updateDirectoryConfigResponse_httpStatus' - The response's http status code.
newUpdateDirectoryConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDirectoryConfigResponse
newUpdateDirectoryConfigResponse pHttpStatus_ =
  UpdateDirectoryConfigResponse'
    { directoryConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Directory Config object.
updateDirectoryConfigResponse_directoryConfig :: Lens.Lens' UpdateDirectoryConfigResponse (Prelude.Maybe DirectoryConfig)
updateDirectoryConfigResponse_directoryConfig = Lens.lens (\UpdateDirectoryConfigResponse' {directoryConfig} -> directoryConfig) (\s@UpdateDirectoryConfigResponse' {} a -> s {directoryConfig = a} :: UpdateDirectoryConfigResponse)

-- | The response's http status code.
updateDirectoryConfigResponse_httpStatus :: Lens.Lens' UpdateDirectoryConfigResponse Prelude.Int
updateDirectoryConfigResponse_httpStatus = Lens.lens (\UpdateDirectoryConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateDirectoryConfigResponse' {} a -> s {httpStatus = a} :: UpdateDirectoryConfigResponse)

instance Prelude.NFData UpdateDirectoryConfigResponse
