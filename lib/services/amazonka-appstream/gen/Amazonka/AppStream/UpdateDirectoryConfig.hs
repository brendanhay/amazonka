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
-- Module      : Amazonka.AppStream.UpdateDirectoryConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified Directory Config object in AppStream 2.0. This
-- object includes the configuration information required to join fleets
-- and image builders to Microsoft Active Directory domains.
module Amazonka.AppStream.UpdateDirectoryConfig
  ( -- * Creating a Request
    UpdateDirectoryConfig (..),
    newUpdateDirectoryConfig,

    -- * Request Lenses
    updateDirectoryConfig_certificateBasedAuthProperties,
    updateDirectoryConfig_organizationalUnitDistinguishedNames,
    updateDirectoryConfig_serviceAccountCredentials,
    updateDirectoryConfig_directoryName,

    -- * Destructuring the Response
    UpdateDirectoryConfigResponse (..),
    newUpdateDirectoryConfigResponse,

    -- * Response Lenses
    updateDirectoryConfigResponse_directoryConfig,
    updateDirectoryConfigResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDirectoryConfig' smart constructor.
data UpdateDirectoryConfig = UpdateDirectoryConfig'
  { -- | The certificate-based authentication properties used to authenticate
    -- SAML 2.0 Identity Provider (IdP) user identities to Active Directory
    -- domain-joined streaming instances. Fallback is turned on by default when
    -- certificate-based authentication is __Enabled__ . Fallback allows users
    -- to log in using their AD domain password if certificate-based
    -- authentication is unsuccessful, or to unlock a desktop lock screen.
    -- __Enabled_no_directory_login_fallback__ enables certificate-based
    -- authentication, but does not allow users to log in using their AD domain
    -- password. Users will be disconnected to re-authenticate using
    -- certificates.
    certificateBasedAuthProperties :: Prelude.Maybe CertificateBasedAuthProperties,
    -- | The distinguished names of the organizational units for computer
    -- accounts.
    organizationalUnitDistinguishedNames :: Prelude.Maybe [Prelude.Text],
    -- | The credentials for the service account used by the fleet or image
    -- builder to connect to the directory.
    serviceAccountCredentials :: Prelude.Maybe ServiceAccountCredentials,
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
-- 'certificateBasedAuthProperties', 'updateDirectoryConfig_certificateBasedAuthProperties' - The certificate-based authentication properties used to authenticate
-- SAML 2.0 Identity Provider (IdP) user identities to Active Directory
-- domain-joined streaming instances. Fallback is turned on by default when
-- certificate-based authentication is __Enabled__ . Fallback allows users
-- to log in using their AD domain password if certificate-based
-- authentication is unsuccessful, or to unlock a desktop lock screen.
-- __Enabled_no_directory_login_fallback__ enables certificate-based
-- authentication, but does not allow users to log in using their AD domain
-- password. Users will be disconnected to re-authenticate using
-- certificates.
--
-- 'organizationalUnitDistinguishedNames', 'updateDirectoryConfig_organizationalUnitDistinguishedNames' - The distinguished names of the organizational units for computer
-- accounts.
--
-- 'serviceAccountCredentials', 'updateDirectoryConfig_serviceAccountCredentials' - The credentials for the service account used by the fleet or image
-- builder to connect to the directory.
--
-- 'directoryName', 'updateDirectoryConfig_directoryName' - The name of the Directory Config object.
newUpdateDirectoryConfig ::
  -- | 'directoryName'
  Prelude.Text ->
  UpdateDirectoryConfig
newUpdateDirectoryConfig pDirectoryName_ =
  UpdateDirectoryConfig'
    { certificateBasedAuthProperties =
        Prelude.Nothing,
      organizationalUnitDistinguishedNames =
        Prelude.Nothing,
      serviceAccountCredentials = Prelude.Nothing,
      directoryName = pDirectoryName_
    }

-- | The certificate-based authentication properties used to authenticate
-- SAML 2.0 Identity Provider (IdP) user identities to Active Directory
-- domain-joined streaming instances. Fallback is turned on by default when
-- certificate-based authentication is __Enabled__ . Fallback allows users
-- to log in using their AD domain password if certificate-based
-- authentication is unsuccessful, or to unlock a desktop lock screen.
-- __Enabled_no_directory_login_fallback__ enables certificate-based
-- authentication, but does not allow users to log in using their AD domain
-- password. Users will be disconnected to re-authenticate using
-- certificates.
updateDirectoryConfig_certificateBasedAuthProperties :: Lens.Lens' UpdateDirectoryConfig (Prelude.Maybe CertificateBasedAuthProperties)
updateDirectoryConfig_certificateBasedAuthProperties = Lens.lens (\UpdateDirectoryConfig' {certificateBasedAuthProperties} -> certificateBasedAuthProperties) (\s@UpdateDirectoryConfig' {} a -> s {certificateBasedAuthProperties = a} :: UpdateDirectoryConfig)

-- | The distinguished names of the organizational units for computer
-- accounts.
updateDirectoryConfig_organizationalUnitDistinguishedNames :: Lens.Lens' UpdateDirectoryConfig (Prelude.Maybe [Prelude.Text])
updateDirectoryConfig_organizationalUnitDistinguishedNames = Lens.lens (\UpdateDirectoryConfig' {organizationalUnitDistinguishedNames} -> organizationalUnitDistinguishedNames) (\s@UpdateDirectoryConfig' {} a -> s {organizationalUnitDistinguishedNames = a} :: UpdateDirectoryConfig) Prelude.. Lens.mapping Lens.coerced

-- | The credentials for the service account used by the fleet or image
-- builder to connect to the directory.
updateDirectoryConfig_serviceAccountCredentials :: Lens.Lens' UpdateDirectoryConfig (Prelude.Maybe ServiceAccountCredentials)
updateDirectoryConfig_serviceAccountCredentials = Lens.lens (\UpdateDirectoryConfig' {serviceAccountCredentials} -> serviceAccountCredentials) (\s@UpdateDirectoryConfig' {} a -> s {serviceAccountCredentials = a} :: UpdateDirectoryConfig)

-- | The name of the Directory Config object.
updateDirectoryConfig_directoryName :: Lens.Lens' UpdateDirectoryConfig Prelude.Text
updateDirectoryConfig_directoryName = Lens.lens (\UpdateDirectoryConfig' {directoryName} -> directoryName) (\s@UpdateDirectoryConfig' {} a -> s {directoryName = a} :: UpdateDirectoryConfig)

instance Core.AWSRequest UpdateDirectoryConfig where
  type
    AWSResponse UpdateDirectoryConfig =
      UpdateDirectoryConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDirectoryConfigResponse'
            Prelude.<$> (x Data..?> "DirectoryConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDirectoryConfig where
  hashWithSalt _salt UpdateDirectoryConfig' {..} =
    _salt
      `Prelude.hashWithSalt` certificateBasedAuthProperties
      `Prelude.hashWithSalt` organizationalUnitDistinguishedNames
      `Prelude.hashWithSalt` serviceAccountCredentials
      `Prelude.hashWithSalt` directoryName

instance Prelude.NFData UpdateDirectoryConfig where
  rnf UpdateDirectoryConfig' {..} =
    Prelude.rnf certificateBasedAuthProperties
      `Prelude.seq` Prelude.rnf organizationalUnitDistinguishedNames
      `Prelude.seq` Prelude.rnf serviceAccountCredentials
      `Prelude.seq` Prelude.rnf directoryName

instance Data.ToHeaders UpdateDirectoryConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.UpdateDirectoryConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDirectoryConfig where
  toJSON UpdateDirectoryConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CertificateBasedAuthProperties" Data..=)
              Prelude.<$> certificateBasedAuthProperties,
            ("OrganizationalUnitDistinguishedNames" Data..=)
              Prelude.<$> organizationalUnitDistinguishedNames,
            ("ServiceAccountCredentials" Data..=)
              Prelude.<$> serviceAccountCredentials,
            Prelude.Just
              ("DirectoryName" Data..= directoryName)
          ]
      )

instance Data.ToPath UpdateDirectoryConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDirectoryConfig where
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

instance Prelude.NFData UpdateDirectoryConfigResponse where
  rnf UpdateDirectoryConfigResponse' {..} =
    Prelude.rnf directoryConfig
      `Prelude.seq` Prelude.rnf httpStatus
