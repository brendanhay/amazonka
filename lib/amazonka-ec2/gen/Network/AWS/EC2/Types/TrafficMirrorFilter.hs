{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorFilter where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TrafficMirrorFilterRule
import Network.AWS.EC2.Types.TrafficMirrorNetworkService
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Traffic Mirror filter.
--
--
--
-- /See:/ 'trafficMirrorFilter' smart constructor.
data TrafficMirrorFilter = TrafficMirrorFilter'
  { _tmfTrafficMirrorFilterId ::
      !(Maybe Text),
    _tmfIngressFilterRules ::
      !(Maybe [TrafficMirrorFilterRule]),
    _tmfNetworkServices ::
      !(Maybe [TrafficMirrorNetworkService]),
    _tmfEgressFilterRules ::
      !(Maybe [TrafficMirrorFilterRule]),
    _tmfDescription :: !(Maybe Text),
    _tmfTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrafficMirrorFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tmfTrafficMirrorFilterId' - The ID of the Traffic Mirror filter.
--
-- * 'tmfIngressFilterRules' - Information about the ingress rules that are associated with the Traffic Mirror filter.
--
-- * 'tmfNetworkServices' - The network service traffic that is associated with the Traffic Mirror filter.
--
-- * 'tmfEgressFilterRules' - Information about the egress rules that are associated with the Traffic Mirror filter.
--
-- * 'tmfDescription' - The description of the Traffic Mirror filter.
--
-- * 'tmfTags' - The tags assigned to the Traffic Mirror filter.
trafficMirrorFilter ::
  TrafficMirrorFilter
trafficMirrorFilter =
  TrafficMirrorFilter'
    { _tmfTrafficMirrorFilterId = Nothing,
      _tmfIngressFilterRules = Nothing,
      _tmfNetworkServices = Nothing,
      _tmfEgressFilterRules = Nothing,
      _tmfDescription = Nothing,
      _tmfTags = Nothing
    }

-- | The ID of the Traffic Mirror filter.
tmfTrafficMirrorFilterId :: Lens' TrafficMirrorFilter (Maybe Text)
tmfTrafficMirrorFilterId = lens _tmfTrafficMirrorFilterId (\s a -> s {_tmfTrafficMirrorFilterId = a})

-- | Information about the ingress rules that are associated with the Traffic Mirror filter.
tmfIngressFilterRules :: Lens' TrafficMirrorFilter [TrafficMirrorFilterRule]
tmfIngressFilterRules = lens _tmfIngressFilterRules (\s a -> s {_tmfIngressFilterRules = a}) . _Default . _Coerce

-- | The network service traffic that is associated with the Traffic Mirror filter.
tmfNetworkServices :: Lens' TrafficMirrorFilter [TrafficMirrorNetworkService]
tmfNetworkServices = lens _tmfNetworkServices (\s a -> s {_tmfNetworkServices = a}) . _Default . _Coerce

-- | Information about the egress rules that are associated with the Traffic Mirror filter.
tmfEgressFilterRules :: Lens' TrafficMirrorFilter [TrafficMirrorFilterRule]
tmfEgressFilterRules = lens _tmfEgressFilterRules (\s a -> s {_tmfEgressFilterRules = a}) . _Default . _Coerce

-- | The description of the Traffic Mirror filter.
tmfDescription :: Lens' TrafficMirrorFilter (Maybe Text)
tmfDescription = lens _tmfDescription (\s a -> s {_tmfDescription = a})

-- | The tags assigned to the Traffic Mirror filter.
tmfTags :: Lens' TrafficMirrorFilter [Tag]
tmfTags = lens _tmfTags (\s a -> s {_tmfTags = a}) . _Default . _Coerce

instance FromXML TrafficMirrorFilter where
  parseXML x =
    TrafficMirrorFilter'
      <$> (x .@? "trafficMirrorFilterId")
      <*> ( x .@? "ingressFilterRuleSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> ( x .@? "networkServiceSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> ( x .@? "egressFilterRuleSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "description")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable TrafficMirrorFilter

instance NFData TrafficMirrorFilter
