{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrefixList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrefixList where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes prefixes for AWS services.
--
--
--
-- /See:/ 'prefixList' smart constructor.
data PrefixList = PrefixList'
  { _plCidrs :: !(Maybe [Text]),
    _plPrefixListId :: !(Maybe Text),
    _plPrefixListName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PrefixList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plCidrs' - The IP address range of the AWS service.
--
-- * 'plPrefixListId' - The ID of the prefix.
--
-- * 'plPrefixListName' - The name of the prefix.
prefixList ::
  PrefixList
prefixList =
  PrefixList'
    { _plCidrs = Nothing,
      _plPrefixListId = Nothing,
      _plPrefixListName = Nothing
    }

-- | The IP address range of the AWS service.
plCidrs :: Lens' PrefixList [Text]
plCidrs = lens _plCidrs (\s a -> s {_plCidrs = a}) . _Default . _Coerce

-- | The ID of the prefix.
plPrefixListId :: Lens' PrefixList (Maybe Text)
plPrefixListId = lens _plPrefixListId (\s a -> s {_plPrefixListId = a})

-- | The name of the prefix.
plPrefixListName :: Lens' PrefixList (Maybe Text)
plPrefixListName = lens _plPrefixListName (\s a -> s {_plPrefixListName = a})

instance FromXML PrefixList where
  parseXML x =
    PrefixList'
      <$> (x .@? "cidrSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "prefixListId")
      <*> (x .@? "prefixListName")

instance Hashable PrefixList

instance NFData PrefixList
