{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TagDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TagDescription where

import Network.AWS.ELBv2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The tags associated with a resource.
--
--
--
-- /See:/ 'tagDescription' smart constructor.
data TagDescription = TagDescription'
  { _tdResourceARN ::
      !(Maybe Text),
    _tdTags :: !(Maybe (List1 Tag))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdResourceARN' - The Amazon Resource Name (ARN) of the resource.
--
-- * 'tdTags' - Information about the tags.
tagDescription ::
  TagDescription
tagDescription =
  TagDescription' {_tdResourceARN = Nothing, _tdTags = Nothing}

-- | The Amazon Resource Name (ARN) of the resource.
tdResourceARN :: Lens' TagDescription (Maybe Text)
tdResourceARN = lens _tdResourceARN (\s a -> s {_tdResourceARN = a})

-- | Information about the tags.
tdTags :: Lens' TagDescription (Maybe (NonEmpty Tag))
tdTags = lens _tdTags (\s a -> s {_tdTags = a}) . mapping _List1

instance FromXML TagDescription where
  parseXML x =
    TagDescription'
      <$> (x .@? "ResourceArn")
      <*> (x .@? "Tags" .!@ mempty >>= may (parseXMLList1 "member"))

instance Hashable TagDescription

instance NFData TagDescription
