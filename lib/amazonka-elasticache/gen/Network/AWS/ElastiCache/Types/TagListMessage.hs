{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.TagListMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.TagListMessage where

import Network.AWS.ElastiCache.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output from the @AddTagsToResource@ , @ListTagsForResource@ , and @RemoveTagsFromResource@ operations.
--
--
--
-- /See:/ 'tagListMessage' smart constructor.
newtype TagListMessage = TagListMessage'
  { _tlmTagList ::
      Maybe [Tag]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagListMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlmTagList' - A list of cost allocation tags as key-value pairs.
tagListMessage ::
  TagListMessage
tagListMessage = TagListMessage' {_tlmTagList = Nothing}

-- | A list of cost allocation tags as key-value pairs.
tlmTagList :: Lens' TagListMessage [Tag]
tlmTagList = lens _tlmTagList (\s a -> s {_tlmTagList = a}) . _Default . _Coerce

instance FromXML TagListMessage where
  parseXML x =
    TagListMessage'
      <$> (x .@? "TagList" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable TagListMessage

instance NFData TagListMessage
