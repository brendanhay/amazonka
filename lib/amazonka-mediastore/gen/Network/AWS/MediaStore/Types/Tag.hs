{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types.Tag where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A collection of tags associated with a container. Each tag consists of a key:value pair, which can be anything you define. Typically, the tag key represents a category (such as "environment") and the tag value represents a specific value within that category (such as "test," "development," or "production"). You can add up to 50 tags to each container. For more information about tagging, including naming and usage conventions, see <https://docs.aws.amazon.com/mediastore/latest/ug/tagging.html Tagging Resources in MediaStore> .
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
-- * 'tagValue' - Part of the key:value pair that defines a tag. You can use a tag value to describe a specific value within a category, such as "companyA" or "companyB." Tag values are case-sensitive.
--
-- * 'tagKey' - Part of the key:value pair that defines a tag. You can use a tag key to describe a category of information, such as "customer." Tag keys are case-sensitive.
tag ::
  -- | 'tagKey'
  Text ->
  Tag
tag pKey_ = Tag' {_tagValue = Nothing, _tagKey = pKey_}

-- | Part of the key:value pair that defines a tag. You can use a tag value to describe a specific value within a category, such as "companyA" or "companyB." Tag values are case-sensitive.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\s a -> s {_tagValue = a})

-- | Part of the key:value pair that defines a tag. You can use a tag key to describe a category of information, such as "customer." Tag keys are case-sensitive.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\s a -> s {_tagKey = a})

instance FromJSON Tag where
  parseJSON =
    withObject
      "Tag"
      (\x -> Tag' <$> (x .:? "Value") <*> (x .: "Key"))

instance Hashable Tag

instance NFData Tag

instance ToJSON Tag where
  toJSON Tag' {..} =
    object
      (catMaybes [("Value" .=) <$> _tagValue, Just ("Key" .= _tagKey)])
