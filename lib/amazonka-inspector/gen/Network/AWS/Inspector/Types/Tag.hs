{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Tag where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A key and value pair. This data type is used as a request parameter in the 'SetTagsForResource' action and a response element in the 'ListTagsForResource' action.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag' {_tagValue :: !(Maybe Text), _tagKey :: !Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - A value assigned to a tag key.
--
-- * 'tagKey' - A tag key.
tag ::
  -- | 'tagKey'
  Text ->
  Tag
tag pKey_ = Tag' {_tagValue = Nothing, _tagKey = pKey_}

-- | A value assigned to a tag key.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\s a -> s {_tagValue = a})

-- | A tag key.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\s a -> s {_tagKey = a})

instance FromJSON Tag where
  parseJSON =
    withObject
      "Tag"
      (\x -> Tag' <$> (x .:? "value") <*> (x .: "key"))

instance Hashable Tag

instance NFData Tag

instance ToJSON Tag where
  toJSON Tag' {..} =
    object
      (catMaybes [("value" .=) <$> _tagValue, Just ("key" .= _tagKey)])
