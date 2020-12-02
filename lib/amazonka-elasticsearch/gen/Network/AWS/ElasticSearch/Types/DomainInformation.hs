{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DomainInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DomainInformation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'domainInformation' smart constructor.
data DomainInformation = DomainInformation'
  { _diOwnerId ::
      !(Maybe Text),
    _diRegion :: !(Maybe Text),
    _diDomainName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diOwnerId' - Undocumented member.
--
-- * 'diRegion' - Undocumented member.
--
-- * 'diDomainName' - Undocumented member.
domainInformation ::
  -- | 'diDomainName'
  Text ->
  DomainInformation
domainInformation pDomainName_ =
  DomainInformation'
    { _diOwnerId = Nothing,
      _diRegion = Nothing,
      _diDomainName = pDomainName_
    }

-- | Undocumented member.
diOwnerId :: Lens' DomainInformation (Maybe Text)
diOwnerId = lens _diOwnerId (\s a -> s {_diOwnerId = a})

-- | Undocumented member.
diRegion :: Lens' DomainInformation (Maybe Text)
diRegion = lens _diRegion (\s a -> s {_diRegion = a})

-- | Undocumented member.
diDomainName :: Lens' DomainInformation Text
diDomainName = lens _diDomainName (\s a -> s {_diDomainName = a})

instance FromJSON DomainInformation where
  parseJSON =
    withObject
      "DomainInformation"
      ( \x ->
          DomainInformation'
            <$> (x .:? "OwnerId") <*> (x .:? "Region") <*> (x .: "DomainName")
      )

instance Hashable DomainInformation

instance NFData DomainInformation

instance ToJSON DomainInformation where
  toJSON DomainInformation' {..} =
    object
      ( catMaybes
          [ ("OwnerId" .=) <$> _diOwnerId,
            ("Region" .=) <$> _diRegion,
            Just ("DomainName" .= _diDomainName)
          ]
      )
