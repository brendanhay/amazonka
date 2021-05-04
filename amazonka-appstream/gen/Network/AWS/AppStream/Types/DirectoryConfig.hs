{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AppStream.Types.DirectoryConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.DirectoryConfig where

import Network.AWS.AppStream.Types.ServiceAccountCredentials
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the configuration information required to join fleets and
-- image builders to Microsoft Active Directory domains.
--
-- /See:/ 'newDirectoryConfig' smart constructor.
data DirectoryConfig = DirectoryConfig'
  { -- | The credentials for the service account used by the fleet or image
    -- builder to connect to the directory.
    serviceAccountCredentials :: Prelude.Maybe ServiceAccountCredentials,
    -- | The time the directory configuration was created.
    createdTime :: Prelude.Maybe Prelude.POSIX,
    -- | The distinguished names of the organizational units for computer
    -- accounts.
    organizationalUnitDistinguishedNames :: Prelude.Maybe [Prelude.Text],
    -- | The fully qualified name of the directory (for example,
    -- corp.example.com).
    directoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DirectoryConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceAccountCredentials', 'directoryConfig_serviceAccountCredentials' - The credentials for the service account used by the fleet or image
-- builder to connect to the directory.
--
-- 'createdTime', 'directoryConfig_createdTime' - The time the directory configuration was created.
--
-- 'organizationalUnitDistinguishedNames', 'directoryConfig_organizationalUnitDistinguishedNames' - The distinguished names of the organizational units for computer
-- accounts.
--
-- 'directoryName', 'directoryConfig_directoryName' - The fully qualified name of the directory (for example,
-- corp.example.com).
newDirectoryConfig ::
  -- | 'directoryName'
  Prelude.Text ->
  DirectoryConfig
newDirectoryConfig pDirectoryName_ =
  DirectoryConfig'
    { serviceAccountCredentials =
        Prelude.Nothing,
      createdTime = Prelude.Nothing,
      organizationalUnitDistinguishedNames =
        Prelude.Nothing,
      directoryName = pDirectoryName_
    }

-- | The credentials for the service account used by the fleet or image
-- builder to connect to the directory.
directoryConfig_serviceAccountCredentials :: Lens.Lens' DirectoryConfig (Prelude.Maybe ServiceAccountCredentials)
directoryConfig_serviceAccountCredentials = Lens.lens (\DirectoryConfig' {serviceAccountCredentials} -> serviceAccountCredentials) (\s@DirectoryConfig' {} a -> s {serviceAccountCredentials = a} :: DirectoryConfig)

-- | The time the directory configuration was created.
directoryConfig_createdTime :: Lens.Lens' DirectoryConfig (Prelude.Maybe Prelude.UTCTime)
directoryConfig_createdTime = Lens.lens (\DirectoryConfig' {createdTime} -> createdTime) (\s@DirectoryConfig' {} a -> s {createdTime = a} :: DirectoryConfig) Prelude.. Lens.mapping Prelude._Time

-- | The distinguished names of the organizational units for computer
-- accounts.
directoryConfig_organizationalUnitDistinguishedNames :: Lens.Lens' DirectoryConfig (Prelude.Maybe [Prelude.Text])
directoryConfig_organizationalUnitDistinguishedNames = Lens.lens (\DirectoryConfig' {organizationalUnitDistinguishedNames} -> organizationalUnitDistinguishedNames) (\s@DirectoryConfig' {} a -> s {organizationalUnitDistinguishedNames = a} :: DirectoryConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | The fully qualified name of the directory (for example,
-- corp.example.com).
directoryConfig_directoryName :: Lens.Lens' DirectoryConfig Prelude.Text
directoryConfig_directoryName = Lens.lens (\DirectoryConfig' {directoryName} -> directoryName) (\s@DirectoryConfig' {} a -> s {directoryName = a} :: DirectoryConfig)

instance Prelude.FromJSON DirectoryConfig where
  parseJSON =
    Prelude.withObject
      "DirectoryConfig"
      ( \x ->
          DirectoryConfig'
            Prelude.<$> (x Prelude..:? "ServiceAccountCredentials")
            Prelude.<*> (x Prelude..:? "CreatedTime")
            Prelude.<*> ( x Prelude..:? "OrganizationalUnitDistinguishedNames"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "DirectoryName")
      )

instance Prelude.Hashable DirectoryConfig

instance Prelude.NFData DirectoryConfig
