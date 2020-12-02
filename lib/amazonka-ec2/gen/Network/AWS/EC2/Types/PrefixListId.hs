{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrefixListId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrefixListId where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a prefix list ID.
--
--
--
-- /See:/ 'prefixListId' smart constructor.
data PrefixListId = PrefixListId'
  { _pliPrefixListId ::
      !(Maybe Text),
    _pliDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PrefixListId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pliPrefixListId' - The ID of the prefix.
--
-- * 'pliDescription' - A description for the security group rule that references this prefix list ID. Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
prefixListId ::
  PrefixListId
prefixListId =
  PrefixListId'
    { _pliPrefixListId = Nothing,
      _pliDescription = Nothing
    }

-- | The ID of the prefix.
pliPrefixListId :: Lens' PrefixListId (Maybe Text)
pliPrefixListId = lens _pliPrefixListId (\s a -> s {_pliPrefixListId = a})

-- | A description for the security group rule that references this prefix list ID. Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
pliDescription :: Lens' PrefixListId (Maybe Text)
pliDescription = lens _pliDescription (\s a -> s {_pliDescription = a})

instance FromXML PrefixListId where
  parseXML x =
    PrefixListId'
      <$> (x .@? "prefixListId") <*> (x .@? "description")

instance Hashable PrefixListId

instance NFData PrefixListId

instance ToQuery PrefixListId where
  toQuery PrefixListId' {..} =
    mconcat
      [ "PrefixListId" =: _pliPrefixListId,
        "Description" =: _pliDescription
      ]
