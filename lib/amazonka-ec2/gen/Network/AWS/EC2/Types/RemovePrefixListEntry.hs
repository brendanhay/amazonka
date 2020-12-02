{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RemovePrefixListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RemovePrefixListEntry where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An entry for a prefix list.
--
--
--
-- /See:/ 'removePrefixListEntry' smart constructor.
newtype RemovePrefixListEntry = RemovePrefixListEntry'
  { _rpleCidr ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemovePrefixListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpleCidr' - The CIDR block.
removePrefixListEntry ::
  -- | 'rpleCidr'
  Text ->
  RemovePrefixListEntry
removePrefixListEntry pCidr_ =
  RemovePrefixListEntry' {_rpleCidr = pCidr_}

-- | The CIDR block.
rpleCidr :: Lens' RemovePrefixListEntry Text
rpleCidr = lens _rpleCidr (\s a -> s {_rpleCidr = a})

instance Hashable RemovePrefixListEntry

instance NFData RemovePrefixListEntry

instance ToQuery RemovePrefixListEntry where
  toQuery RemovePrefixListEntry' {..} = mconcat ["Cidr" =: _rpleCidr]
