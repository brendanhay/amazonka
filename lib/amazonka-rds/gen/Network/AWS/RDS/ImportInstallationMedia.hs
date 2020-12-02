{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ImportInstallationMedia
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the installation media for a DB engine that requires an on-premises customer provided license, such as SQL Server.
module Network.AWS.RDS.ImportInstallationMedia
  ( -- * Creating a Request
    importInstallationMedia,
    ImportInstallationMedia,

    -- * Request Lenses
    iimCustomAvailabilityZoneId,
    iimEngine,
    iimEngineVersion,
    iimEngineInstallationMediaPath,
    iimOSInstallationMediaPath,

    -- * Destructuring the Response
    installationMedia,
    InstallationMedia,

    -- * Response Lenses
    imEngineVersion,
    imStatus,
    imInstallationMediaId,
    imEngineInstallationMediaPath,
    imEngine,
    imOSInstallationMediaPath,
    imCustomAvailabilityZoneId,
    imFailureCause,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'importInstallationMedia' smart constructor.
data ImportInstallationMedia = ImportInstallationMedia'
  { _iimCustomAvailabilityZoneId ::
      !Text,
    _iimEngine :: !Text,
    _iimEngineVersion :: !Text,
    _iimEngineInstallationMediaPath :: !Text,
    _iimOSInstallationMediaPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportInstallationMedia' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iimCustomAvailabilityZoneId' - The identifier of the custom Availability Zone (AZ) to import the installation media to.
--
-- * 'iimEngine' - The name of the database engine to be used for this instance.  The list only includes supported DB engines that require an on-premises customer provided license.  Valid Values:      * @sqlserver-ee@      * @sqlserver-se@      * @sqlserver-ex@      * @sqlserver-web@
--
-- * 'iimEngineVersion' - The version number of the database engine to use. For a list of valid engine versions, call 'DescribeDBEngineVersions' . The following are the database engines and links to information about the major and minor versions. The list only includes DB engines that require an on-premises customer provided license. __Microsoft SQL Server__  See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS> in the /Amazon RDS User Guide./
--
-- * 'iimEngineInstallationMediaPath' - The path to the installation medium for the specified DB engine. Example: @SQLServerISO/en_sql_server_2016_enterprise_x64_dvd_8701793.iso@
--
-- * 'iimOSInstallationMediaPath' - The path to the installation medium for the operating system associated with the specified DB engine. Example: @WindowsISO/en_windows_server_2016_x64_dvd_9327751.iso@
importInstallationMedia ::
  -- | 'iimCustomAvailabilityZoneId'
  Text ->
  -- | 'iimEngine'
  Text ->
  -- | 'iimEngineVersion'
  Text ->
  -- | 'iimEngineInstallationMediaPath'
  Text ->
  -- | 'iimOSInstallationMediaPath'
  Text ->
  ImportInstallationMedia
importInstallationMedia
  pCustomAvailabilityZoneId_
  pEngine_
  pEngineVersion_
  pEngineInstallationMediaPath_
  pOSInstallationMediaPath_ =
    ImportInstallationMedia'
      { _iimCustomAvailabilityZoneId =
          pCustomAvailabilityZoneId_,
        _iimEngine = pEngine_,
        _iimEngineVersion = pEngineVersion_,
        _iimEngineInstallationMediaPath = pEngineInstallationMediaPath_,
        _iimOSInstallationMediaPath = pOSInstallationMediaPath_
      }

-- | The identifier of the custom Availability Zone (AZ) to import the installation media to.
iimCustomAvailabilityZoneId :: Lens' ImportInstallationMedia Text
iimCustomAvailabilityZoneId = lens _iimCustomAvailabilityZoneId (\s a -> s {_iimCustomAvailabilityZoneId = a})

-- | The name of the database engine to be used for this instance.  The list only includes supported DB engines that require an on-premises customer provided license.  Valid Values:      * @sqlserver-ee@      * @sqlserver-se@      * @sqlserver-ex@      * @sqlserver-web@
iimEngine :: Lens' ImportInstallationMedia Text
iimEngine = lens _iimEngine (\s a -> s {_iimEngine = a})

-- | The version number of the database engine to use. For a list of valid engine versions, call 'DescribeDBEngineVersions' . The following are the database engines and links to information about the major and minor versions. The list only includes DB engines that require an on-premises customer provided license. __Microsoft SQL Server__  See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS> in the /Amazon RDS User Guide./
iimEngineVersion :: Lens' ImportInstallationMedia Text
iimEngineVersion = lens _iimEngineVersion (\s a -> s {_iimEngineVersion = a})

-- | The path to the installation medium for the specified DB engine. Example: @SQLServerISO/en_sql_server_2016_enterprise_x64_dvd_8701793.iso@
iimEngineInstallationMediaPath :: Lens' ImportInstallationMedia Text
iimEngineInstallationMediaPath = lens _iimEngineInstallationMediaPath (\s a -> s {_iimEngineInstallationMediaPath = a})

-- | The path to the installation medium for the operating system associated with the specified DB engine. Example: @WindowsISO/en_windows_server_2016_x64_dvd_9327751.iso@
iimOSInstallationMediaPath :: Lens' ImportInstallationMedia Text
iimOSInstallationMediaPath = lens _iimOSInstallationMediaPath (\s a -> s {_iimOSInstallationMediaPath = a})

instance AWSRequest ImportInstallationMedia where
  type Rs ImportInstallationMedia = InstallationMedia
  request = postQuery rds
  response =
    receiveXMLWrapper
      "ImportInstallationMediaResult"
      (\s h x -> parseXML x)

instance Hashable ImportInstallationMedia

instance NFData ImportInstallationMedia

instance ToHeaders ImportInstallationMedia where
  toHeaders = const mempty

instance ToPath ImportInstallationMedia where
  toPath = const "/"

instance ToQuery ImportInstallationMedia where
  toQuery ImportInstallationMedia' {..} =
    mconcat
      [ "Action" =: ("ImportInstallationMedia" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "CustomAvailabilityZoneId" =: _iimCustomAvailabilityZoneId,
        "Engine" =: _iimEngine,
        "EngineVersion" =: _iimEngineVersion,
        "EngineInstallationMediaPath" =: _iimEngineInstallationMediaPath,
        "OSInstallationMediaPath" =: _iimOSInstallationMediaPath
      ]
