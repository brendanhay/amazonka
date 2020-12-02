{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.TagDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.TagDescription where

import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The tags associated with a load balancer.
--
--
--
-- /See:/ 'tagDescription' smart constructor.
data TagDescription = TagDescription'
  { _tdLoadBalancerName ::
      !(Maybe Text),
    _tdTags :: !(Maybe (List1 Tag))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdLoadBalancerName' - The name of the load balancer.
--
-- * 'tdTags' - The tags.
tagDescription ::
  TagDescription
tagDescription =
  TagDescription' {_tdLoadBalancerName = Nothing, _tdTags = Nothing}

-- | The name of the load balancer.
tdLoadBalancerName :: Lens' TagDescription (Maybe Text)
tdLoadBalancerName = lens _tdLoadBalancerName (\s a -> s {_tdLoadBalancerName = a})

-- | The tags.
tdTags :: Lens' TagDescription (Maybe (NonEmpty Tag))
tdTags = lens _tdTags (\s a -> s {_tdTags = a}) . mapping _List1

instance FromXML TagDescription where
  parseXML x =
    TagDescription'
      <$> (x .@? "LoadBalancerName")
      <*> (x .@? "Tags" .!@ mempty >>= may (parseXMLList1 "member"))

instance Hashable TagDescription

instance NFData TagDescription
