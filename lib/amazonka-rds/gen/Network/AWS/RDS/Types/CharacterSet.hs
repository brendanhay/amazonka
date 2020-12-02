{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.CharacterSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.CharacterSet where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data type is used as a response element in the action @DescribeDBEngineVersions@ .
--
--
--
-- /See:/ 'characterSet' smart constructor.
data CharacterSet = CharacterSet'
  { _csCharacterSetName ::
      !(Maybe Text),
    _csCharacterSetDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CharacterSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCharacterSetName' - The name of the character set.
--
-- * 'csCharacterSetDescription' - The description of the character set.
characterSet ::
  CharacterSet
characterSet =
  CharacterSet'
    { _csCharacterSetName = Nothing,
      _csCharacterSetDescription = Nothing
    }

-- | The name of the character set.
csCharacterSetName :: Lens' CharacterSet (Maybe Text)
csCharacterSetName = lens _csCharacterSetName (\s a -> s {_csCharacterSetName = a})

-- | The description of the character set.
csCharacterSetDescription :: Lens' CharacterSet (Maybe Text)
csCharacterSetDescription = lens _csCharacterSetDescription (\s a -> s {_csCharacterSetDescription = a})

instance FromXML CharacterSet where
  parseXML x =
    CharacterSet'
      <$> (x .@? "CharacterSetName") <*> (x .@? "CharacterSetDescription")

instance Hashable CharacterSet

instance NFData CharacterSet
