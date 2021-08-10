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
-- Module      : Network.AWS.AppStream.CreateDirectoryConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Directory Config object in AppStream 2.0. This object includes
-- the configuration information required to join fleets and image builders
-- to Microsoft Active Directory domains.
module Network.AWS.AppStream.CreateDirectoryConfig
  ( -- * Creating a Request
    CreateDirectoryConfig (..),
    newCreateDirectoryConfig,

    -- * Request Lenses
    createDirectoryConfig_serviceAccountCredentials,
    createDirectoryConfig_directoryName,
    createDirectoryConfig_organizationalUnitDistinguishedNames,

    -- * Destructuring the Response
    CreateDirectoryConfigResponse (..),
    newCreateDirectoryConfigResponse,

    -- * Response Lenses
    createDirectoryConfigResponse_directoryConfig,
    createDirectoryConfigResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDirectoryConfig' smart constructor.
data CreateDirectoryConfig = CreateDirectoryConfig'
  { -- | The credentials for the service account used by the fleet or image
    -- builder to connect to the directory.
    serviceAccountCredentials :: Prelude.Maybe ServiceAccountCredentials,
    -- | The fully qualified name of the directory (for example,
    -- corp.example.com).
    directoryName :: Prelude.Text,
    -- | The distinguished names of the organizational units for computer
    -- accounts.
    organizationalUnitDistinguishedNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDirectoryConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceAccountCredentials', 'createDirectoryConfig_serviceAccountCredentials' - The credentials for the service account used by the fleet or image
-- builder to connect to the directory.
--
-- 'directoryName', 'createDirectoryConfig_directoryName' - The fully qualified name of the directory (for example,
-- corp.example.com).
--
-- 'organizationalUnitDistinguishedNames', 'createDirectoryConfig_organizationalUnitDistinguishedNames' - The distinguished names of the organizational units for computer
-- accounts.
newCreateDirectoryConfig ::
  -- | 'directoryName'
  Prelude.Text ->
  CreateDirectoryConfig
newCreateDirectoryConfig pDirectoryName_ =
  CreateDirectoryConfig'
    { serviceAccountCredentials =
        Prelude.Nothing,
      directoryName = pDirectoryName_,
      organizationalUnitDistinguishedNames =
        Prelude.mempty
    }

-- | The credentials for the service account used by the fleet or image
-- builder to connect to the directory.
createDirectoryConfig_serviceAccountCredentials :: Lens.Lens' CreateDirectoryConfig (Prelude.Maybe ServiceAccountCredentials)
createDirectoryConfig_serviceAccountCredentials = Lens.lens (\CreateDirectoryConfig' {serviceAccountCredentials} -> serviceAccountCredentials) (\s@CreateDirectoryConfig' {} a -> s {serviceAccountCredentials = a} :: CreateDirectoryConfig)

-- | The fully qualified name of the directory (for example,
-- corp.example.com).
createDirectoryConfig_directoryName :: Lens.Lens' CreateDirectoryConfig Prelude.Text
createDirectoryConfig_directoryName = Lens.lens (\CreateDirectoryConfig' {directoryName} -> directoryName) (\s@CreateDirectoryConfig' {} a -> s {directoryName = a} :: CreateDirectoryConfig)

-- | The distinguished names of the organizational units for computer
-- accounts.
createDirectoryConfig_organizationalUnitDistinguishedNames :: Lens.Lens' CreateDirectoryConfig [Prelude.Text]
createDirectoryConfig_organizationalUnitDistinguishedNames = Lens.lens (\CreateDirectoryConfig' {organizationalUnitDistinguishedNames} -> organizationalUnitDistinguishedNames) (\s@CreateDirectoryConfig' {} a -> s {organizationalUnitDistinguishedNames = a} :: CreateDirectoryConfig) Prelude.. Lens._Coerce

instance Core.AWSRequest CreateDirectoryConfig where
  type
    AWSResponse CreateDirectoryConfig =
      CreateDirectoryConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDirectoryConfigResponse'
            Prelude.<$> (x Core..?> "DirectoryConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDirectoryConfig

instance Prelude.NFData CreateDirectoryConfig

instance Core.ToHeaders CreateDirectoryConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.CreateDirectoryConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDirectoryConfig where
  toJSON CreateDirectoryConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ServiceAccountCredentials" Core..=)
              Prelude.<$> serviceAccountCredentials,
            Prelude.Just ("DirectoryName" Core..= directoryName),
            Prelude.Just
              ( "OrganizationalUnitDistinguishedNames"
                  Core..= organizationalUnitDistinguishedNames
              )
          ]
      )

instance Core.ToPath CreateDirectoryConfig where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDirectoryConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDirectoryConfigResponse' smart constructor.
data CreateDirectoryConfigResponse = CreateDirectoryConfigResponse'
  { -- | Information about the directory configuration.
    directoryConfig :: Prelude.Maybe DirectoryConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDirectoryConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryConfig', 'createDirectoryConfigResponse_directoryConfig' - Information about the directory configuration.
--
-- 'httpStatus', 'createDirectoryConfigResponse_httpStatus' - The response's http status code.
newCreateDirectoryConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDirectoryConfigResponse
newCreateDirectoryConfigResponse pHttpStatus_ =
  CreateDirectoryConfigResponse'
    { directoryConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the directory configuration.
createDirectoryConfigResponse_directoryConfig :: Lens.Lens' CreateDirectoryConfigResponse (Prelude.Maybe DirectoryConfig)
createDirectoryConfigResponse_directoryConfig = Lens.lens (\CreateDirectoryConfigResponse' {directoryConfig} -> directoryConfig) (\s@CreateDirectoryConfigResponse' {} a -> s {directoryConfig = a} :: CreateDirectoryConfigResponse)

-- | The response's http status code.
createDirectoryConfigResponse_httpStatus :: Lens.Lens' CreateDirectoryConfigResponse Prelude.Int
createDirectoryConfigResponse_httpStatus = Lens.lens (\CreateDirectoryConfigResponse' {httpStatus} -> httpStatus) (\s@CreateDirectoryConfigResponse' {} a -> s {httpStatus = a} :: CreateDirectoryConfigResponse)

instance Prelude.NFData CreateDirectoryConfigResponse
