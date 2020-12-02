{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.TagInfoForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.TagInfoForResource where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.Tag

-- | Information for a tag associated with an AWS resource. Tags are key:value pairs that you can use to categorize and manage your resources, for purposes like billing. For example, you might set the tag key to "customer" and the value to the customer name or ID. You can specify one or more tags to add to each AWS resource, up to 50 tags for a resource.
--
--
-- Tagging is only available through the API, SDKs, and CLI. You can't manage or view tags through the AWS WAF Classic console. You can tag the AWS resources that you manage through AWS WAF Classic: web ACLs, rule groups, and rules.
--
--
-- /See:/ 'tagInfoForResource' smart constructor.
data TagInfoForResource = TagInfoForResource'
  { _tifrTagList ::
      !(Maybe (List1 Tag)),
    _tifrResourceARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagInfoForResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tifrTagList' -
--
-- * 'tifrResourceARN' -
tagInfoForResource ::
  TagInfoForResource
tagInfoForResource =
  TagInfoForResource'
    { _tifrTagList = Nothing,
      _tifrResourceARN = Nothing
    }

-- |
tifrTagList :: Lens' TagInfoForResource (Maybe (NonEmpty Tag))
tifrTagList = lens _tifrTagList (\s a -> s {_tifrTagList = a}) . mapping _List1

-- |
tifrResourceARN :: Lens' TagInfoForResource (Maybe Text)
tifrResourceARN = lens _tifrResourceARN (\s a -> s {_tifrResourceARN = a})

instance FromJSON TagInfoForResource where
  parseJSON =
    withObject
      "TagInfoForResource"
      ( \x ->
          TagInfoForResource'
            <$> (x .:? "TagList") <*> (x .:? "ResourceARN")
      )

instance Hashable TagInfoForResource

instance NFData TagInfoForResource
