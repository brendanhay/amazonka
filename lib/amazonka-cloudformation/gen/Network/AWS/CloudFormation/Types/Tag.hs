{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Tag where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Tag type enables you to specify a key-value pair that can be used to store information about an AWS CloudFormation stack.
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
-- * 'tagKey' - /Required/ . A string used to identify this tag. You can specify a maximum of 128 characters for a tag key. Tags owned by Amazon Web Services (AWS) have the reserved prefix: @aws:@ .
--
-- * 'tagValue' - /Required/ . A string containing the value for this tag. You can specify a maximum of 256 characters for a tag value.
tag ::
  -- | 'tagKey'
  Text ->
  -- | 'tagValue'
  Text ->
  Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}

-- | /Required/ . A string used to identify this tag. You can specify a maximum of 128 characters for a tag key. Tags owned by Amazon Web Services (AWS) have the reserved prefix: @aws:@ .
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\s a -> s {_tagKey = a})

-- | /Required/ . A string containing the value for this tag. You can specify a maximum of 256 characters for a tag value.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\s a -> s {_tagValue = a})

instance FromXML Tag where
  parseXML x = Tag' <$> (x .@ "Key") <*> (x .@ "Value")

instance Hashable Tag

instance NFData Tag

instance ToQuery Tag where
  toQuery Tag' {..} = mconcat ["Key" =: _tagKey, "Value" =: _tagValue]
