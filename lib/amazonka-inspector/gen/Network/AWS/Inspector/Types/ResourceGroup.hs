{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.ResourceGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.ResourceGroup where

import Network.AWS.Inspector.Types.ResourceGroupTag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a resource group. The resource group defines a set of tags that, when queried, identify the AWS resources that make up the assessment target. This data type is used as the response element in the 'DescribeResourceGroups' action.
--
--
--
-- /See:/ 'resourceGroup' smart constructor.
data ResourceGroup = ResourceGroup'
  { _rgArn :: !Text,
    _rgTags :: !(List1 ResourceGroupTag),
    _rgCreatedAt :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgArn' - The ARN of the resource group.
--
-- * 'rgTags' - The tags (key and value pairs) of the resource group. This data type property is used in the 'CreateResourceGroup' action.
--
-- * 'rgCreatedAt' - The time at which resource group is created.
resourceGroup ::
  -- | 'rgArn'
  Text ->
  -- | 'rgTags'
  NonEmpty ResourceGroupTag ->
  -- | 'rgCreatedAt'
  UTCTime ->
  ResourceGroup
resourceGroup pArn_ pTags_ pCreatedAt_ =
  ResourceGroup'
    { _rgArn = pArn_,
      _rgTags = _List1 # pTags_,
      _rgCreatedAt = _Time # pCreatedAt_
    }

-- | The ARN of the resource group.
rgArn :: Lens' ResourceGroup Text
rgArn = lens _rgArn (\s a -> s {_rgArn = a})

-- | The tags (key and value pairs) of the resource group. This data type property is used in the 'CreateResourceGroup' action.
rgTags :: Lens' ResourceGroup (NonEmpty ResourceGroupTag)
rgTags = lens _rgTags (\s a -> s {_rgTags = a}) . _List1

-- | The time at which resource group is created.
rgCreatedAt :: Lens' ResourceGroup UTCTime
rgCreatedAt = lens _rgCreatedAt (\s a -> s {_rgCreatedAt = a}) . _Time

instance FromJSON ResourceGroup where
  parseJSON =
    withObject
      "ResourceGroup"
      ( \x ->
          ResourceGroup'
            <$> (x .: "arn") <*> (x .: "tags") <*> (x .: "createdAt")
      )

instance Hashable ResourceGroup

instance NFData ResourceGroup
