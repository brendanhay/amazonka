{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancerState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.LoadBalancerState where

import Network.AWS.ELBv2.Types.LoadBalancerStateEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the state of the load balancer.
--
--
--
-- /See:/ 'loadBalancerState' smart constructor.
data LoadBalancerState = LoadBalancerState'
  { _lbsReason ::
      !(Maybe Text),
    _lbsCode :: !(Maybe LoadBalancerStateEnum)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoadBalancerState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbsReason' - A description of the state.
--
-- * 'lbsCode' - The state code. The initial state of the load balancer is @provisioning@ . After the load balancer is fully set up and ready to route traffic, its state is @active@ . If the load balancer could not be set up, its state is @failed@ .
loadBalancerState ::
  LoadBalancerState
loadBalancerState =
  LoadBalancerState' {_lbsReason = Nothing, _lbsCode = Nothing}

-- | A description of the state.
lbsReason :: Lens' LoadBalancerState (Maybe Text)
lbsReason = lens _lbsReason (\s a -> s {_lbsReason = a})

-- | The state code. The initial state of the load balancer is @provisioning@ . After the load balancer is fully set up and ready to route traffic, its state is @active@ . If the load balancer could not be set up, its state is @failed@ .
lbsCode :: Lens' LoadBalancerState (Maybe LoadBalancerStateEnum)
lbsCode = lens _lbsCode (\s a -> s {_lbsCode = a})

instance FromXML LoadBalancerState where
  parseXML x =
    LoadBalancerState' <$> (x .@? "Reason") <*> (x .@? "Code")

instance Hashable LoadBalancerState

instance NFData LoadBalancerState
