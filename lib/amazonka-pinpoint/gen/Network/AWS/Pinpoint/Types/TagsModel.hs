{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TagsModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TagsModel where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the tags (keys and values) for an application, campaign, message template, or segment.
--
--
--
-- /See:/ 'tagsModel' smart constructor.
newtype TagsModel = TagsModel' {_tmTags :: Map Text (Text)}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagsModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tmTags' - A string-to-string map of key-value pairs that defines the tags for an application, campaign, message template, or segment. Each of these resources can have a maximum of 50 tags. Each tag consists of a required tag key and an associated tag value. The maximum length of a tag key is 128 characters. The maximum length of a tag value is 256 characters.
tagsModel ::
  TagsModel
tagsModel = TagsModel' {_tmTags = mempty}

-- | A string-to-string map of key-value pairs that defines the tags for an application, campaign, message template, or segment. Each of these resources can have a maximum of 50 tags. Each tag consists of a required tag key and an associated tag value. The maximum length of a tag key is 128 characters. The maximum length of a tag value is 256 characters.
tmTags :: Lens' TagsModel (HashMap Text (Text))
tmTags = lens _tmTags (\s a -> s {_tmTags = a}) . _Map

instance FromJSON TagsModel where
  parseJSON =
    withObject
      "TagsModel"
      (\x -> TagsModel' <$> (x .:? "tags" .!= mempty))

instance Hashable TagsModel

instance NFData TagsModel

instance ToJSON TagsModel where
  toJSON TagsModel' {..} =
    object (catMaybes [Just ("tags" .= _tmTags)])
