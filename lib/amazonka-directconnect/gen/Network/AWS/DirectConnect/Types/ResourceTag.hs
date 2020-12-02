{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.ResourceTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.ResourceTag where

import Network.AWS.DirectConnect.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a tag associated with an AWS Direct Connect resource.
--
--
--
-- /See:/ 'resourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { _rtResourceARN :: !(Maybe Text),
    _rtTags :: !(Maybe (List1 Tag))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtResourceARN' - The Amazon Resource Name (ARN) of the resource.
--
-- * 'rtTags' - The tags.
resourceTag ::
  ResourceTag
resourceTag =
  ResourceTag' {_rtResourceARN = Nothing, _rtTags = Nothing}

-- | The Amazon Resource Name (ARN) of the resource.
rtResourceARN :: Lens' ResourceTag (Maybe Text)
rtResourceARN = lens _rtResourceARN (\s a -> s {_rtResourceARN = a})

-- | The tags.
rtTags :: Lens' ResourceTag (Maybe (NonEmpty Tag))
rtTags = lens _rtTags (\s a -> s {_rtTags = a}) . mapping _List1

instance FromJSON ResourceTag where
  parseJSON =
    withObject
      "ResourceTag"
      (\x -> ResourceTag' <$> (x .:? "resourceArn") <*> (x .:? "tags"))

instance Hashable ResourceTag

instance NFData ResourceTag
