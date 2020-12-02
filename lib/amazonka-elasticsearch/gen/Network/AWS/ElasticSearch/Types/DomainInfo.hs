{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DomainInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DomainInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'domainInfo' smart constructor.
newtype DomainInfo = DomainInfo' {_dDomainName :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDomainName' - Specifies the @DomainName@ .
domainInfo ::
  DomainInfo
domainInfo = DomainInfo' {_dDomainName = Nothing}

-- | Specifies the @DomainName@ .
dDomainName :: Lens' DomainInfo (Maybe Text)
dDomainName = lens _dDomainName (\s a -> s {_dDomainName = a})

instance FromJSON DomainInfo where
  parseJSON =
    withObject
      "DomainInfo"
      (\x -> DomainInfo' <$> (x .:? "DomainName"))

instance Hashable DomainInfo

instance NFData DomainInfo
