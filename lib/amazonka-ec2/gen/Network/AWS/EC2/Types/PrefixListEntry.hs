{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrefixListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrefixListEntry where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a prefix list entry.
--
--
--
-- /See:/ 'prefixListEntry' smart constructor.
data PrefixListEntry = PrefixListEntry'
  { _pleCidr :: !(Maybe Text),
    _pleDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PrefixListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pleCidr' - The CIDR block.
--
-- * 'pleDescription' - The description.
prefixListEntry ::
  PrefixListEntry
prefixListEntry =
  PrefixListEntry' {_pleCidr = Nothing, _pleDescription = Nothing}

-- | The CIDR block.
pleCidr :: Lens' PrefixListEntry (Maybe Text)
pleCidr = lens _pleCidr (\s a -> s {_pleCidr = a})

-- | The description.
pleDescription :: Lens' PrefixListEntry (Maybe Text)
pleDescription = lens _pleDescription (\s a -> s {_pleDescription = a})

instance FromXML PrefixListEntry where
  parseXML x =
    PrefixListEntry' <$> (x .@? "cidr") <*> (x .@? "description")

instance Hashable PrefixListEntry

instance NFData PrefixListEntry
