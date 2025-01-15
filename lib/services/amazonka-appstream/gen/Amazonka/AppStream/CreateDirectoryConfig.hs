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
-- Module      : Amazonka.AppStream.CreateDirectoryConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Directory Config object in AppStream 2.0. This object includes
-- the configuration information required to join fleets and image builders
-- to Microsoft Active Directory domains.
module Amazonka.AppStream.CreateDirectoryConfig
  ( -- * Creating a Request
    CreateDirectoryConfig (..),
    newCreateDirectoryConfig,

    -- * Request Lenses
    createDirectoryConfig_certificateBasedAuthProperties,
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

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDirectoryConfig' smart constructor.
data CreateDirectoryConfig = CreateDirectoryConfig'
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
    -- | The credentials for the service account used by the fleet or image
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
-- 'certificateBasedAuthProperties', 'createDirectoryConfig_certificateBasedAuthProperties' - The certificate-based authentication properties used to authenticate
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
    { certificateBasedAuthProperties =
        Prelude.Nothing,
      serviceAccountCredentials = Prelude.Nothing,
      directoryName = pDirectoryName_,
      organizationalUnitDistinguishedNames =
        Prelude.mempty
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
createDirectoryConfig_certificateBasedAuthProperties :: Lens.Lens' CreateDirectoryConfig (Prelude.Maybe CertificateBasedAuthProperties)
createDirectoryConfig_certificateBasedAuthProperties = Lens.lens (\CreateDirectoryConfig' {certificateBasedAuthProperties} -> certificateBasedAuthProperties) (\s@CreateDirectoryConfig' {} a -> s {certificateBasedAuthProperties = a} :: CreateDirectoryConfig)

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
createDirectoryConfig_organizationalUnitDistinguishedNames = Lens.lens (\CreateDirectoryConfig' {organizationalUnitDistinguishedNames} -> organizationalUnitDistinguishedNames) (\s@CreateDirectoryConfig' {} a -> s {organizationalUnitDistinguishedNames = a} :: CreateDirectoryConfig) Prelude.. Lens.coerced

instance Core.AWSRequest CreateDirectoryConfig where
  type
    AWSResponse CreateDirectoryConfig =
      CreateDirectoryConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDirectoryConfigResponse'
            Prelude.<$> (x Data..?> "DirectoryConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDirectoryConfig where
  hashWithSalt _salt CreateDirectoryConfig' {..} =
    _salt
      `Prelude.hashWithSalt` certificateBasedAuthProperties
      `Prelude.hashWithSalt` serviceAccountCredentials
      `Prelude.hashWithSalt` directoryName
      `Prelude.hashWithSalt` organizationalUnitDistinguishedNames

instance Prelude.NFData CreateDirectoryConfig where
  rnf CreateDirectoryConfig' {..} =
    Prelude.rnf certificateBasedAuthProperties `Prelude.seq`
      Prelude.rnf serviceAccountCredentials `Prelude.seq`
        Prelude.rnf directoryName `Prelude.seq`
          Prelude.rnf organizationalUnitDistinguishedNames

instance Data.ToHeaders CreateDirectoryConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.CreateDirectoryConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDirectoryConfig where
  toJSON CreateDirectoryConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CertificateBasedAuthProperties" Data..=)
              Prelude.<$> certificateBasedAuthProperties,
            ("ServiceAccountCredentials" Data..=)
              Prelude.<$> serviceAccountCredentials,
            Prelude.Just ("DirectoryName" Data..= directoryName),
            Prelude.Just
              ( "OrganizationalUnitDistinguishedNames"
                  Data..= organizationalUnitDistinguishedNames
              )
          ]
      )

instance Data.ToPath CreateDirectoryConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDirectoryConfig where
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

instance Prelude.NFData CreateDirectoryConfigResponse where
  rnf CreateDirectoryConfigResponse' {..} =
    Prelude.rnf directoryConfig `Prelude.seq`
      Prelude.rnf httpStatus
