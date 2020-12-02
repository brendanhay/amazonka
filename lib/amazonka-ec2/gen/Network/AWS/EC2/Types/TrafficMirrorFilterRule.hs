{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorFilterRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorFilterRule where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TrafficDirection
import Network.AWS.EC2.Types.TrafficMirrorPortRange
import Network.AWS.EC2.Types.TrafficMirrorRuleAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Traffic Mirror rule.
--
--
--
-- /See:/ 'trafficMirrorFilterRule' smart constructor.
data TrafficMirrorFilterRule = TrafficMirrorFilterRule'
  { _tmfrRuleNumber ::
      !(Maybe Int),
    _tmfrTrafficDirection ::
      !(Maybe TrafficDirection),
    _tmfrRuleAction ::
      !(Maybe TrafficMirrorRuleAction),
    _tmfrProtocol :: !(Maybe Int),
    _tmfrTrafficMirrorFilterId :: !(Maybe Text),
    _tmfrTrafficMirrorFilterRuleId ::
      !(Maybe Text),
    _tmfrDestinationPortRange ::
      !(Maybe TrafficMirrorPortRange),
    _tmfrSourceCidrBlock :: !(Maybe Text),
    _tmfrSourcePortRange ::
      !(Maybe TrafficMirrorPortRange),
    _tmfrDescription :: !(Maybe Text),
    _tmfrDestinationCidrBlock :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrafficMirrorFilterRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tmfrRuleNumber' - The rule number of the Traffic Mirror rule.
--
-- * 'tmfrTrafficDirection' - The traffic direction assigned to the Traffic Mirror rule.
--
-- * 'tmfrRuleAction' - The action assigned to the Traffic Mirror rule.
--
-- * 'tmfrProtocol' - The protocol assigned to the Traffic Mirror rule.
--
-- * 'tmfrTrafficMirrorFilterId' - The ID of the Traffic Mirror filter that the rule is associated with.
--
-- * 'tmfrTrafficMirrorFilterRuleId' - The ID of the Traffic Mirror rule.
--
-- * 'tmfrDestinationPortRange' - The destination port range assigned to the Traffic Mirror rule.
--
-- * 'tmfrSourceCidrBlock' - The source CIDR block assigned to the Traffic Mirror rule.
--
-- * 'tmfrSourcePortRange' - The source port range assigned to the Traffic Mirror rule.
--
-- * 'tmfrDescription' - The description of the Traffic Mirror rule.
--
-- * 'tmfrDestinationCidrBlock' - The destination CIDR block assigned to the Traffic Mirror rule.
trafficMirrorFilterRule ::
  TrafficMirrorFilterRule
trafficMirrorFilterRule =
  TrafficMirrorFilterRule'
    { _tmfrRuleNumber = Nothing,
      _tmfrTrafficDirection = Nothing,
      _tmfrRuleAction = Nothing,
      _tmfrProtocol = Nothing,
      _tmfrTrafficMirrorFilterId = Nothing,
      _tmfrTrafficMirrorFilterRuleId = Nothing,
      _tmfrDestinationPortRange = Nothing,
      _tmfrSourceCidrBlock = Nothing,
      _tmfrSourcePortRange = Nothing,
      _tmfrDescription = Nothing,
      _tmfrDestinationCidrBlock = Nothing
    }

-- | The rule number of the Traffic Mirror rule.
tmfrRuleNumber :: Lens' TrafficMirrorFilterRule (Maybe Int)
tmfrRuleNumber = lens _tmfrRuleNumber (\s a -> s {_tmfrRuleNumber = a})

-- | The traffic direction assigned to the Traffic Mirror rule.
tmfrTrafficDirection :: Lens' TrafficMirrorFilterRule (Maybe TrafficDirection)
tmfrTrafficDirection = lens _tmfrTrafficDirection (\s a -> s {_tmfrTrafficDirection = a})

-- | The action assigned to the Traffic Mirror rule.
tmfrRuleAction :: Lens' TrafficMirrorFilterRule (Maybe TrafficMirrorRuleAction)
tmfrRuleAction = lens _tmfrRuleAction (\s a -> s {_tmfrRuleAction = a})

-- | The protocol assigned to the Traffic Mirror rule.
tmfrProtocol :: Lens' TrafficMirrorFilterRule (Maybe Int)
tmfrProtocol = lens _tmfrProtocol (\s a -> s {_tmfrProtocol = a})

-- | The ID of the Traffic Mirror filter that the rule is associated with.
tmfrTrafficMirrorFilterId :: Lens' TrafficMirrorFilterRule (Maybe Text)
tmfrTrafficMirrorFilterId = lens _tmfrTrafficMirrorFilterId (\s a -> s {_tmfrTrafficMirrorFilterId = a})

-- | The ID of the Traffic Mirror rule.
tmfrTrafficMirrorFilterRuleId :: Lens' TrafficMirrorFilterRule (Maybe Text)
tmfrTrafficMirrorFilterRuleId = lens _tmfrTrafficMirrorFilterRuleId (\s a -> s {_tmfrTrafficMirrorFilterRuleId = a})

-- | The destination port range assigned to the Traffic Mirror rule.
tmfrDestinationPortRange :: Lens' TrafficMirrorFilterRule (Maybe TrafficMirrorPortRange)
tmfrDestinationPortRange = lens _tmfrDestinationPortRange (\s a -> s {_tmfrDestinationPortRange = a})

-- | The source CIDR block assigned to the Traffic Mirror rule.
tmfrSourceCidrBlock :: Lens' TrafficMirrorFilterRule (Maybe Text)
tmfrSourceCidrBlock = lens _tmfrSourceCidrBlock (\s a -> s {_tmfrSourceCidrBlock = a})

-- | The source port range assigned to the Traffic Mirror rule.
tmfrSourcePortRange :: Lens' TrafficMirrorFilterRule (Maybe TrafficMirrorPortRange)
tmfrSourcePortRange = lens _tmfrSourcePortRange (\s a -> s {_tmfrSourcePortRange = a})

-- | The description of the Traffic Mirror rule.
tmfrDescription :: Lens' TrafficMirrorFilterRule (Maybe Text)
tmfrDescription = lens _tmfrDescription (\s a -> s {_tmfrDescription = a})

-- | The destination CIDR block assigned to the Traffic Mirror rule.
tmfrDestinationCidrBlock :: Lens' TrafficMirrorFilterRule (Maybe Text)
tmfrDestinationCidrBlock = lens _tmfrDestinationCidrBlock (\s a -> s {_tmfrDestinationCidrBlock = a})

instance FromXML TrafficMirrorFilterRule where
  parseXML x =
    TrafficMirrorFilterRule'
      <$> (x .@? "ruleNumber")
      <*> (x .@? "trafficDirection")
      <*> (x .@? "ruleAction")
      <*> (x .@? "protocol")
      <*> (x .@? "trafficMirrorFilterId")
      <*> (x .@? "trafficMirrorFilterRuleId")
      <*> (x .@? "destinationPortRange")
      <*> (x .@? "sourceCidrBlock")
      <*> (x .@? "sourcePortRange")
      <*> (x .@? "description")
      <*> (x .@? "destinationCidrBlock")

instance Hashable TrafficMirrorFilterRule

instance NFData TrafficMirrorFilterRule
