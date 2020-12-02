{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.Tag where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The metadata that you apply to AWS resources to help you categorize and organize them. Each tag consists of a key and a value, both of which you define. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ .
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag' {_tagKey :: !Text, _tagValue :: !Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - One part of a key-value pair that makes up a tag. A key is a general label that acts like a category for more specific tag values.
--
-- * 'tagValue' - One part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key). The value can be empty or null.
tag ::
  -- | 'tagKey'
  Text ->
  -- | 'tagValue'
  Text ->
  Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}

-- | One part of a key-value pair that makes up a tag. A key is a general label that acts like a category for more specific tag values.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\s a -> s {_tagKey = a})

-- | One part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key). The value can be empty or null.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\s a -> s {_tagValue = a})

instance FromJSON Tag where
  parseJSON =
    withObject
      "Tag"
      (\x -> Tag' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable Tag

instance NFData Tag
