{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.Tag where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A tag, consisting of a key and a value.
--
--
-- This tag is available for use by AWS services that support tags in AWS CodeBuild.
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
-- * 'tagValue' - The tag's value.
--
-- * 'tagKey' - The tag's key.
tag ::
  Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}

-- | The tag's value.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\s a -> s {_tagValue = a})

-- | The tag's key.
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
