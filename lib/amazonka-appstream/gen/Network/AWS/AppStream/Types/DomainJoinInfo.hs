{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.DomainJoinInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.DomainJoinInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the configuration information required to join fleets and image builders to Microsoft Active Directory domains.
--
--
--
-- /See:/ 'domainJoinInfo' smart constructor.
data DomainJoinInfo = DomainJoinInfo'
  { _djiOrganizationalUnitDistinguishedName ::
      !(Maybe Text),
    _djiDirectoryName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainJoinInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djiOrganizationalUnitDistinguishedName' - The distinguished name of the organizational unit for computer accounts.
--
-- * 'djiDirectoryName' - The fully qualified name of the directory (for example, corp.example.com).
domainJoinInfo ::
  DomainJoinInfo
domainJoinInfo =
  DomainJoinInfo'
    { _djiOrganizationalUnitDistinguishedName =
        Nothing,
      _djiDirectoryName = Nothing
    }

-- | The distinguished name of the organizational unit for computer accounts.
djiOrganizationalUnitDistinguishedName :: Lens' DomainJoinInfo (Maybe Text)
djiOrganizationalUnitDistinguishedName = lens _djiOrganizationalUnitDistinguishedName (\s a -> s {_djiOrganizationalUnitDistinguishedName = a})

-- | The fully qualified name of the directory (for example, corp.example.com).
djiDirectoryName :: Lens' DomainJoinInfo (Maybe Text)
djiDirectoryName = lens _djiDirectoryName (\s a -> s {_djiDirectoryName = a})

instance FromJSON DomainJoinInfo where
  parseJSON =
    withObject
      "DomainJoinInfo"
      ( \x ->
          DomainJoinInfo'
            <$> (x .:? "OrganizationalUnitDistinguishedName")
            <*> (x .:? "DirectoryName")
      )

instance Hashable DomainJoinInfo

instance NFData DomainJoinInfo

instance ToJSON DomainJoinInfo where
  toJSON DomainJoinInfo' {..} =
    object
      ( catMaybes
          [ ("OrganizationalUnitDistinguishedName" .=)
              <$> _djiOrganizationalUnitDistinguishedName,
            ("DirectoryName" .=) <$> _djiDirectoryName
          ]
      )
