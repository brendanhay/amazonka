{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppStream.Types.DirectoryConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.DirectoryConfig where

import Amazonka.AppStream.Types.CertificateBasedAuthProperties
import Amazonka.AppStream.Types.ServiceAccountCredentials
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration information required to join fleets and
-- image builders to Microsoft Active Directory domains.
--
-- /See:/ 'newDirectoryConfig' smart constructor.
data DirectoryConfig = DirectoryConfig'
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
    -- | The time the directory configuration was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The distinguished names of the organizational units for computer
    -- accounts.
    organizationalUnitDistinguishedNames :: Prelude.Maybe [Prelude.Text],
    -- | The credentials for the service account used by the fleet or image
    -- builder to connect to the directory.
    serviceAccountCredentials :: Prelude.Maybe ServiceAccountCredentials,
    -- | The fully qualified name of the directory (for example,
    -- corp.example.com).
    directoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DirectoryConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateBasedAuthProperties', 'directoryConfig_certificateBasedAuthProperties' - The certificate-based authentication properties used to authenticate
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
-- 'createdTime', 'directoryConfig_createdTime' - The time the directory configuration was created.
--
-- 'organizationalUnitDistinguishedNames', 'directoryConfig_organizationalUnitDistinguishedNames' - The distinguished names of the organizational units for computer
-- accounts.
--
-- 'serviceAccountCredentials', 'directoryConfig_serviceAccountCredentials' - The credentials for the service account used by the fleet or image
-- builder to connect to the directory.
--
-- 'directoryName', 'directoryConfig_directoryName' - The fully qualified name of the directory (for example,
-- corp.example.com).
newDirectoryConfig ::
  -- | 'directoryName'
  Prelude.Text ->
  DirectoryConfig
newDirectoryConfig pDirectoryName_ =
  DirectoryConfig'
    { certificateBasedAuthProperties =
        Prelude.Nothing,
      createdTime = Prelude.Nothing,
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
directoryConfig_certificateBasedAuthProperties :: Lens.Lens' DirectoryConfig (Prelude.Maybe CertificateBasedAuthProperties)
directoryConfig_certificateBasedAuthProperties = Lens.lens (\DirectoryConfig' {certificateBasedAuthProperties} -> certificateBasedAuthProperties) (\s@DirectoryConfig' {} a -> s {certificateBasedAuthProperties = a} :: DirectoryConfig)

-- | The time the directory configuration was created.
directoryConfig_createdTime :: Lens.Lens' DirectoryConfig (Prelude.Maybe Prelude.UTCTime)
directoryConfig_createdTime = Lens.lens (\DirectoryConfig' {createdTime} -> createdTime) (\s@DirectoryConfig' {} a -> s {createdTime = a} :: DirectoryConfig) Prelude.. Lens.mapping Data._Time

-- | The distinguished names of the organizational units for computer
-- accounts.
directoryConfig_organizationalUnitDistinguishedNames :: Lens.Lens' DirectoryConfig (Prelude.Maybe [Prelude.Text])
directoryConfig_organizationalUnitDistinguishedNames = Lens.lens (\DirectoryConfig' {organizationalUnitDistinguishedNames} -> organizationalUnitDistinguishedNames) (\s@DirectoryConfig' {} a -> s {organizationalUnitDistinguishedNames = a} :: DirectoryConfig) Prelude.. Lens.mapping Lens.coerced

-- | The credentials for the service account used by the fleet or image
-- builder to connect to the directory.
directoryConfig_serviceAccountCredentials :: Lens.Lens' DirectoryConfig (Prelude.Maybe ServiceAccountCredentials)
directoryConfig_serviceAccountCredentials = Lens.lens (\DirectoryConfig' {serviceAccountCredentials} -> serviceAccountCredentials) (\s@DirectoryConfig' {} a -> s {serviceAccountCredentials = a} :: DirectoryConfig)

-- | The fully qualified name of the directory (for example,
-- corp.example.com).
directoryConfig_directoryName :: Lens.Lens' DirectoryConfig Prelude.Text
directoryConfig_directoryName = Lens.lens (\DirectoryConfig' {directoryName} -> directoryName) (\s@DirectoryConfig' {} a -> s {directoryName = a} :: DirectoryConfig)

instance Data.FromJSON DirectoryConfig where
  parseJSON =
    Data.withObject
      "DirectoryConfig"
      ( \x ->
          DirectoryConfig'
            Prelude.<$> (x Data..:? "CertificateBasedAuthProperties")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> ( x
                            Data..:? "OrganizationalUnitDistinguishedNames"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ServiceAccountCredentials")
            Prelude.<*> (x Data..: "DirectoryName")
      )

instance Prelude.Hashable DirectoryConfig where
  hashWithSalt _salt DirectoryConfig' {..} =
    _salt
      `Prelude.hashWithSalt` certificateBasedAuthProperties
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` organizationalUnitDistinguishedNames
      `Prelude.hashWithSalt` serviceAccountCredentials
      `Prelude.hashWithSalt` directoryName

instance Prelude.NFData DirectoryConfig where
  rnf DirectoryConfig' {..} =
    Prelude.rnf certificateBasedAuthProperties
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf organizationalUnitDistinguishedNames
      `Prelude.seq` Prelude.rnf serviceAccountCredentials
      `Prelude.seq` Prelude.rnf directoryName
