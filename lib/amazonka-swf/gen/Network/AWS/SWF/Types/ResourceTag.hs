{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ResourceTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ResourceTag where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Tags are key-value pairs that can be associated with Amazon SWF state machines and activities.
--
--
-- Tags may only contain unicode letters, digits, whitespace, or these symbols: @_ . : / = + - @@ .
--
--
-- /See:/ 'resourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { _rtValue :: !(Maybe Text),
    _rtKey :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtValue' - The value of a tag.
--
-- * 'rtKey' - The key of a tag.
resourceTag ::
  -- | 'rtKey'
  Text ->
  ResourceTag
resourceTag pKey_ =
  ResourceTag' {_rtValue = Nothing, _rtKey = pKey_}

-- | The value of a tag.
rtValue :: Lens' ResourceTag (Maybe Text)
rtValue = lens _rtValue (\s a -> s {_rtValue = a})

-- | The key of a tag.
rtKey :: Lens' ResourceTag Text
rtKey = lens _rtKey (\s a -> s {_rtKey = a})

instance FromJSON ResourceTag where
  parseJSON =
    withObject
      "ResourceTag"
      (\x -> ResourceTag' <$> (x .:? "value") <*> (x .: "key"))

instance Hashable ResourceTag

instance NFData ResourceTag

instance ToJSON ResourceTag where
  toJSON ResourceTag' {..} =
    object
      (catMaybes [("value" .=) <$> _rtValue, Just ("key" .= _rtKey)])
