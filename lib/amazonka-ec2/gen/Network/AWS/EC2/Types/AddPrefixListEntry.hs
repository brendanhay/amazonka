{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AddPrefixListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AddPrefixListEntry where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An entry for a prefix list.
--
--
--
-- /See:/ 'addPrefixListEntry' smart constructor.
data AddPrefixListEntry = AddPrefixListEntry'
  { _apleDescription ::
      !(Maybe Text),
    _apleCidr :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AddPrefixListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apleDescription' - A description for the entry. Constraints: Up to 255 characters in length.
--
-- * 'apleCidr' - The CIDR block.
addPrefixListEntry ::
  -- | 'apleCidr'
  Text ->
  AddPrefixListEntry
addPrefixListEntry pCidr_ =
  AddPrefixListEntry'
    { _apleDescription = Nothing,
      _apleCidr = pCidr_
    }

-- | A description for the entry. Constraints: Up to 255 characters in length.
apleDescription :: Lens' AddPrefixListEntry (Maybe Text)
apleDescription = lens _apleDescription (\s a -> s {_apleDescription = a})

-- | The CIDR block.
apleCidr :: Lens' AddPrefixListEntry Text
apleCidr = lens _apleCidr (\s a -> s {_apleCidr = a})

instance Hashable AddPrefixListEntry

instance NFData AddPrefixListEntry

instance ToQuery AddPrefixListEntry where
  toQuery AddPrefixListEntry' {..} =
    mconcat ["Description" =: _apleDescription, "Cidr" =: _apleCidr]
