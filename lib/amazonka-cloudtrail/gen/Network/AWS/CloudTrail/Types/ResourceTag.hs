{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.ResourceTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.ResourceTag where

import Network.AWS.CloudTrail.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A resource tag.
--
--
--
-- /See:/ 'resourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { _rResourceId :: !(Maybe Text),
    _rTagsList :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rResourceId' - Specifies the ARN of the resource.
--
-- * 'rTagsList' - A list of tags.
resourceTag ::
  ResourceTag
resourceTag =
  ResourceTag' {_rResourceId = Nothing, _rTagsList = Nothing}

-- | Specifies the ARN of the resource.
rResourceId :: Lens' ResourceTag (Maybe Text)
rResourceId = lens _rResourceId (\s a -> s {_rResourceId = a})

-- | A list of tags.
rTagsList :: Lens' ResourceTag [Tag]
rTagsList = lens _rTagsList (\s a -> s {_rTagsList = a}) . _Default . _Coerce

instance FromJSON ResourceTag where
  parseJSON =
    withObject
      "ResourceTag"
      ( \x ->
          ResourceTag'
            <$> (x .:? "ResourceId") <*> (x .:? "TagsList" .!= mempty)
      )

instance Hashable ResourceTag

instance NFData ResourceTag
