{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ResourceTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ResourceTag where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The resource tags that AWS Firewall Manager uses to determine if a particular resource should be included or excluded from the AWS Firewall Manager policy. Tags enable you to categorize your AWS resources in different ways, for example, by purpose, owner, or environment. Each tag consists of a key and an optional value. Firewall Manager combines the tags with "AND" so that, if you add more than one tag to a policy scope, a resource must have all the specified tags to be included or excluded. For more information, see <https://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/tag-editor.html Working with Tag Editor> .
--
--
--
-- /See:/ 'resourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { _rtValue :: !(Maybe Text),
    _rtKey :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtValue' - The resource tag value.
--
-- * 'rtKey' - The resource tag key.
resourceTag ::
  -- | 'rtKey'
  Text ->
  ResourceTag
resourceTag pKey_ =
  ResourceTag' {_rtValue = Nothing, _rtKey = pKey_}

-- | The resource tag value.
rtValue :: Lens' ResourceTag (Maybe Text)
rtValue = lens _rtValue (\s a -> s {_rtValue = a})

-- | The resource tag key.
rtKey :: Lens' ResourceTag Text
rtKey = lens _rtKey (\s a -> s {_rtKey = a})

instance FromJSON ResourceTag where
  parseJSON =
    withObject
      "ResourceTag"
      (\x -> ResourceTag' <$> (x .:? "Value") <*> (x .: "Key"))

instance Hashable ResourceTag

instance NFData ResourceTag

instance ToJSON ResourceTag where
  toJSON ResourceTag' {..} =
    object
      (catMaybes [("Value" .=) <$> _rtValue, Just ("Key" .= _rtKey)])
