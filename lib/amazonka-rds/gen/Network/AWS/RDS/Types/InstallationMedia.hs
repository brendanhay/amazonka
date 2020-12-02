{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.InstallationMedia
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.InstallationMedia where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.InstallationMediaFailureCause

-- | Contains the installation media for a DB engine that requires an on-premises customer provided license, such as Microsoft SQL Server.
--
--
--
-- /See:/ 'installationMedia' smart constructor.
data InstallationMedia = InstallationMedia'
  { _imEngineVersion ::
      !(Maybe Text),
    _imStatus :: !(Maybe Text),
    _imInstallationMediaId :: !(Maybe Text),
    _imEngineInstallationMediaPath :: !(Maybe Text),
    _imEngine :: !(Maybe Text),
    _imOSInstallationMediaPath :: !(Maybe Text),
    _imCustomAvailabilityZoneId :: !(Maybe Text),
    _imFailureCause ::
      !(Maybe InstallationMediaFailureCause)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstallationMedia' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imEngineVersion' - The engine version of the DB engine.
--
-- * 'imStatus' - The status of the installation medium.
--
-- * 'imInstallationMediaId' - The installation medium ID.
--
-- * 'imEngineInstallationMediaPath' - The path to the installation medium for the DB engine.
--
-- * 'imEngine' - The DB engine.
--
-- * 'imOSInstallationMediaPath' - The path to the installation medium for the operating system associated with the DB engine.
--
-- * 'imCustomAvailabilityZoneId' - The custom Availability Zone (AZ) that contains the installation media.
--
-- * 'imFailureCause' - If an installation media failure occurred, the cause of the failure.
installationMedia ::
  InstallationMedia
installationMedia =
  InstallationMedia'
    { _imEngineVersion = Nothing,
      _imStatus = Nothing,
      _imInstallationMediaId = Nothing,
      _imEngineInstallationMediaPath = Nothing,
      _imEngine = Nothing,
      _imOSInstallationMediaPath = Nothing,
      _imCustomAvailabilityZoneId = Nothing,
      _imFailureCause = Nothing
    }

-- | The engine version of the DB engine.
imEngineVersion :: Lens' InstallationMedia (Maybe Text)
imEngineVersion = lens _imEngineVersion (\s a -> s {_imEngineVersion = a})

-- | The status of the installation medium.
imStatus :: Lens' InstallationMedia (Maybe Text)
imStatus = lens _imStatus (\s a -> s {_imStatus = a})

-- | The installation medium ID.
imInstallationMediaId :: Lens' InstallationMedia (Maybe Text)
imInstallationMediaId = lens _imInstallationMediaId (\s a -> s {_imInstallationMediaId = a})

-- | The path to the installation medium for the DB engine.
imEngineInstallationMediaPath :: Lens' InstallationMedia (Maybe Text)
imEngineInstallationMediaPath = lens _imEngineInstallationMediaPath (\s a -> s {_imEngineInstallationMediaPath = a})

-- | The DB engine.
imEngine :: Lens' InstallationMedia (Maybe Text)
imEngine = lens _imEngine (\s a -> s {_imEngine = a})

-- | The path to the installation medium for the operating system associated with the DB engine.
imOSInstallationMediaPath :: Lens' InstallationMedia (Maybe Text)
imOSInstallationMediaPath = lens _imOSInstallationMediaPath (\s a -> s {_imOSInstallationMediaPath = a})

-- | The custom Availability Zone (AZ) that contains the installation media.
imCustomAvailabilityZoneId :: Lens' InstallationMedia (Maybe Text)
imCustomAvailabilityZoneId = lens _imCustomAvailabilityZoneId (\s a -> s {_imCustomAvailabilityZoneId = a})

-- | If an installation media failure occurred, the cause of the failure.
imFailureCause :: Lens' InstallationMedia (Maybe InstallationMediaFailureCause)
imFailureCause = lens _imFailureCause (\s a -> s {_imFailureCause = a})

instance FromXML InstallationMedia where
  parseXML x =
    InstallationMedia'
      <$> (x .@? "EngineVersion")
      <*> (x .@? "Status")
      <*> (x .@? "InstallationMediaId")
      <*> (x .@? "EngineInstallationMediaPath")
      <*> (x .@? "Engine")
      <*> (x .@? "OSInstallationMediaPath")
      <*> (x .@? "CustomAvailabilityZoneId")
      <*> (x .@? "FailureCause")

instance Hashable InstallationMedia

instance NFData InstallationMedia
