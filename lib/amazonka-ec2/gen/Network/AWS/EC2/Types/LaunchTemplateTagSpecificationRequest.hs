{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateTagSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateTagSpecificationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ResourceType
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The tags specification for the launch template.
--
--
--
-- /See:/ 'launchTemplateTagSpecificationRequest' smart constructor.
data LaunchTemplateTagSpecificationRequest = LaunchTemplateTagSpecificationRequest'
  { _lttsrResourceType ::
      !( Maybe
           ResourceType
       ),
    _lttsrTags ::
      !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplateTagSpecificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lttsrResourceType' - The type of resource to tag. Currently, the resource types that support tagging on creation are @instance@ and @volume@ . To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
--
-- * 'lttsrTags' - The tags to apply to the resource.
launchTemplateTagSpecificationRequest ::
  LaunchTemplateTagSpecificationRequest
launchTemplateTagSpecificationRequest =
  LaunchTemplateTagSpecificationRequest'
    { _lttsrResourceType =
        Nothing,
      _lttsrTags = Nothing
    }

-- | The type of resource to tag. Currently, the resource types that support tagging on creation are @instance@ and @volume@ . To tag a resource after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
lttsrResourceType :: Lens' LaunchTemplateTagSpecificationRequest (Maybe ResourceType)
lttsrResourceType = lens _lttsrResourceType (\s a -> s {_lttsrResourceType = a})

-- | The tags to apply to the resource.
lttsrTags :: Lens' LaunchTemplateTagSpecificationRequest [Tag]
lttsrTags = lens _lttsrTags (\s a -> s {_lttsrTags = a}) . _Default . _Coerce

instance Hashable LaunchTemplateTagSpecificationRequest

instance NFData LaunchTemplateTagSpecificationRequest

instance ToQuery LaunchTemplateTagSpecificationRequest where
  toQuery LaunchTemplateTagSpecificationRequest' {..} =
    mconcat
      [ "ResourceType" =: _lttsrResourceType,
        toQuery (toQueryList "Tag" <$> _lttsrTags)
      ]
