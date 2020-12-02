{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.Tag where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A key-value pair. A tag consists of a tag key and a tag value. Tag keys and tag values are both required, but tag values can be empty (null) strings.
--
--
-- For information about the rules that apply to tag keys and tag values, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html User-Defined Tag Restrictions> in the /AWS Billing and Cost Management User Guide/ .
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag' {_tagTagKey :: !Text, _tagTagValue :: !Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagTagKey' - The key of the tag.
--
-- * 'tagTagValue' - The value of the tag.
tag ::
  -- | 'tagTagKey'
  Text ->
  -- | 'tagTagValue'
  Text ->
  Tag
tag pTagKey_ pTagValue_ =
  Tag' {_tagTagKey = pTagKey_, _tagTagValue = pTagValue_}

-- | The key of the tag.
tagTagKey :: Lens' Tag Text
tagTagKey = lens _tagTagKey (\s a -> s {_tagTagKey = a})

-- | The value of the tag.
tagTagValue :: Lens' Tag Text
tagTagValue = lens _tagTagValue (\s a -> s {_tagTagValue = a})

instance FromJSON Tag where
  parseJSON =
    withObject
      "Tag"
      (\x -> Tag' <$> (x .: "TagKey") <*> (x .: "TagValue"))

instance Hashable Tag

instance NFData Tag

instance ToJSON Tag where
  toJSON Tag' {..} =
    object
      ( catMaybes
          [Just ("TagKey" .= _tagTagKey), Just ("TagValue" .= _tagTagValue)]
      )
