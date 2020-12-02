{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.RecordTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.RecordTag where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a tag, which is a key-value pair.
--
--
--
-- /See:/ 'recordTag' smart constructor.
data RecordTag = RecordTag'
  { _rtValue :: !(Maybe Text),
    _rtKey :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RecordTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtValue' - The value for this tag.
--
-- * 'rtKey' - The key for this tag.
recordTag ::
  RecordTag
recordTag = RecordTag' {_rtValue = Nothing, _rtKey = Nothing}

-- | The value for this tag.
rtValue :: Lens' RecordTag (Maybe Text)
rtValue = lens _rtValue (\s a -> s {_rtValue = a})

-- | The key for this tag.
rtKey :: Lens' RecordTag (Maybe Text)
rtKey = lens _rtKey (\s a -> s {_rtKey = a})

instance FromJSON RecordTag where
  parseJSON =
    withObject
      "RecordTag"
      (\x -> RecordTag' <$> (x .:? "Value") <*> (x .:? "Key"))

instance Hashable RecordTag

instance NFData RecordTag
