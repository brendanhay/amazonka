{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.KeyListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.KeyListEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about each entry in the key list.
--
--
--
-- /See:/ 'keyListEntry' smart constructor.
data KeyListEntry = KeyListEntry'
  { _kleKeyId :: !(Maybe Text),
    _kleKeyARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeyListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kleKeyId' - Unique identifier of the key.
--
-- * 'kleKeyARN' - ARN of the key.
keyListEntry ::
  KeyListEntry
keyListEntry =
  KeyListEntry' {_kleKeyId = Nothing, _kleKeyARN = Nothing}

-- | Unique identifier of the key.
kleKeyId :: Lens' KeyListEntry (Maybe Text)
kleKeyId = lens _kleKeyId (\s a -> s {_kleKeyId = a})

-- | ARN of the key.
kleKeyARN :: Lens' KeyListEntry (Maybe Text)
kleKeyARN = lens _kleKeyARN (\s a -> s {_kleKeyARN = a})

instance FromJSON KeyListEntry where
  parseJSON =
    withObject
      "KeyListEntry"
      (\x -> KeyListEntry' <$> (x .:? "KeyId") <*> (x .:? "KeyArn"))

instance Hashable KeyListEntry

instance NFData KeyListEntry
