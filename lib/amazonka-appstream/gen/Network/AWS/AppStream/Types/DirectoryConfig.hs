{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.DirectoryConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.DirectoryConfig where

import Network.AWS.AppStream.Types.ServiceAccountCredentials
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the configuration information required to join fleets and image builders to Microsoft Active Directory domains.
--
--
--
-- /See:/ 'directoryConfig' smart constructor.
data DirectoryConfig = DirectoryConfig'
  { _dcCreatedTime ::
      !(Maybe POSIX),
    _dcServiceAccountCredentials ::
      !(Maybe ServiceAccountCredentials),
    _dcOrganizationalUnitDistinguishedNames :: !(Maybe [Text]),
    _dcDirectoryName :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DirectoryConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcCreatedTime' - The time the directory configuration was created.
--
-- * 'dcServiceAccountCredentials' - The credentials for the service account used by the fleet or image builder to connect to the directory.
--
-- * 'dcOrganizationalUnitDistinguishedNames' - The distinguished names of the organizational units for computer accounts.
--
-- * 'dcDirectoryName' - The fully qualified name of the directory (for example, corp.example.com).
directoryConfig ::
  -- | 'dcDirectoryName'
  Text ->
  DirectoryConfig
directoryConfig pDirectoryName_ =
  DirectoryConfig'
    { _dcCreatedTime = Nothing,
      _dcServiceAccountCredentials = Nothing,
      _dcOrganizationalUnitDistinguishedNames = Nothing,
      _dcDirectoryName = pDirectoryName_
    }

-- | The time the directory configuration was created.
dcCreatedTime :: Lens' DirectoryConfig (Maybe UTCTime)
dcCreatedTime = lens _dcCreatedTime (\s a -> s {_dcCreatedTime = a}) . mapping _Time

-- | The credentials for the service account used by the fleet or image builder to connect to the directory.
dcServiceAccountCredentials :: Lens' DirectoryConfig (Maybe ServiceAccountCredentials)
dcServiceAccountCredentials = lens _dcServiceAccountCredentials (\s a -> s {_dcServiceAccountCredentials = a})

-- | The distinguished names of the organizational units for computer accounts.
dcOrganizationalUnitDistinguishedNames :: Lens' DirectoryConfig [Text]
dcOrganizationalUnitDistinguishedNames = lens _dcOrganizationalUnitDistinguishedNames (\s a -> s {_dcOrganizationalUnitDistinguishedNames = a}) . _Default . _Coerce

-- | The fully qualified name of the directory (for example, corp.example.com).
dcDirectoryName :: Lens' DirectoryConfig Text
dcDirectoryName = lens _dcDirectoryName (\s a -> s {_dcDirectoryName = a})

instance FromJSON DirectoryConfig where
  parseJSON =
    withObject
      "DirectoryConfig"
      ( \x ->
          DirectoryConfig'
            <$> (x .:? "CreatedTime")
            <*> (x .:? "ServiceAccountCredentials")
            <*> (x .:? "OrganizationalUnitDistinguishedNames" .!= mempty)
            <*> (x .: "DirectoryName")
      )

instance Hashable DirectoryConfig

instance NFData DirectoryConfig
