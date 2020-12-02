{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Tag where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The metadata that you apply to a resource to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
--
--
-- The following basic restrictions apply to tags:
--
--     * Maximum number of tags per resource - 50
--
--     * For each resource, each tag key must be unique, and each tag key can have only one value.
--
--     * Maximum key length - 128 Unicode characters in UTF-8
--
--     * Maximum value length - 256 Unicode characters in UTF-8
--
--     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
--
--     * Tag keys and values are case-sensitive.
--
--     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
--
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text),
    _tagKey :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - The optional part of a key-value pair that make up a tag. A @value@ acts as a descriptor within a tag category (key).
--
-- * 'tagKey' - One part of a key-value pair that make up a tag. A @key@ is a general label that acts like a category for more specific tag values.
tag ::
  Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}

-- | The optional part of a key-value pair that make up a tag. A @value@ acts as a descriptor within a tag category (key).
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\s a -> s {_tagValue = a})

-- | One part of a key-value pair that make up a tag. A @key@ is a general label that acts like a category for more specific tag values.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\s a -> s {_tagKey = a})

instance FromJSON Tag where
  parseJSON =
    withObject
      "Tag"
      (\x -> Tag' <$> (x .:? "value") <*> (x .:? "key"))

instance Hashable Tag

instance NFData Tag

instance ToJSON Tag where
  toJSON Tag' {..} =
    object
      (catMaybes [("value" .=) <$> _tagValue, ("key" .=) <$> _tagKey])
