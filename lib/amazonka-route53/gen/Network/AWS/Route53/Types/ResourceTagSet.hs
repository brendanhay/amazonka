{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ResourceTagSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ResourceTagSet where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.Tag
import Network.AWS.Route53.Types.TagResourceType

-- | A complex type containing a resource and its associated tags.
--
--
--
-- /See:/ 'resourceTagSet' smart constructor.
data ResourceTagSet = ResourceTagSet'
  { _rtsResourceId ::
      !(Maybe Text),
    _rtsResourceType :: !(Maybe TagResourceType),
    _rtsTags :: !(Maybe (List1 Tag))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceTagSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtsResourceId' - The ID for the specified resource.
--
-- * 'rtsResourceType' - The type of the resource.     * The resource type for health checks is @healthcheck@ .     * The resource type for hosted zones is @hostedzone@ .
--
-- * 'rtsTags' - The tags associated with the specified resource.
resourceTagSet ::
  ResourceTagSet
resourceTagSet =
  ResourceTagSet'
    { _rtsResourceId = Nothing,
      _rtsResourceType = Nothing,
      _rtsTags = Nothing
    }

-- | The ID for the specified resource.
rtsResourceId :: Lens' ResourceTagSet (Maybe Text)
rtsResourceId = lens _rtsResourceId (\s a -> s {_rtsResourceId = a})

-- | The type of the resource.     * The resource type for health checks is @healthcheck@ .     * The resource type for hosted zones is @hostedzone@ .
rtsResourceType :: Lens' ResourceTagSet (Maybe TagResourceType)
rtsResourceType = lens _rtsResourceType (\s a -> s {_rtsResourceType = a})

-- | The tags associated with the specified resource.
rtsTags :: Lens' ResourceTagSet (Maybe (NonEmpty Tag))
rtsTags = lens _rtsTags (\s a -> s {_rtsTags = a}) . mapping _List1

instance FromXML ResourceTagSet where
  parseXML x =
    ResourceTagSet'
      <$> (x .@? "ResourceId")
      <*> (x .@? "ResourceType")
      <*> (x .@? "Tags" .!@ mempty >>= may (parseXMLList1 "Tag"))

instance Hashable ResourceTagSet

instance NFData ResourceTagSet
