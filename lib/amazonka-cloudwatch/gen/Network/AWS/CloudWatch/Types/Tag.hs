{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.Tag where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A key-value pair associated with a CloudWatch resource.
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
-- * 'tagKey' - A string that you can use to assign a value. The combination of tag keys and values can help you organize and categorize your resources.
--
-- * 'tagValue' - The value for the specified tag key.
tag ::
  -- | 'tagKey'
  Text ->
  -- | 'tagValue'
  Text ->
  Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}

-- | A string that you can use to assign a value. The combination of tag keys and values can help you organize and categorize your resources.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\s a -> s {_tagKey = a})

-- | The value for the specified tag key.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\s a -> s {_tagValue = a})

instance FromXML Tag where
  parseXML x = Tag' <$> (x .@ "Key") <*> (x .@ "Value")

instance Hashable Tag

instance NFData Tag

instance ToQuery Tag where
  toQuery Tag' {..} = mconcat ["Key" =: _tagKey, "Value" =: _tagValue]
