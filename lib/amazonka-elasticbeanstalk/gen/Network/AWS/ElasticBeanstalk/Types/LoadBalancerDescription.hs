{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.LoadBalancerDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.LoadBalancerDescription where

import Network.AWS.ElasticBeanstalk.Types.Listener
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the details of a LoadBalancer.
--
--
--
-- /See:/ 'loadBalancerDescription' smart constructor.
data LoadBalancerDescription = LoadBalancerDescription'
  { _lbdLoadBalancerName ::
      !(Maybe Text),
    _lbdDomain :: !(Maybe Text),
    _lbdListeners :: !(Maybe [Listener])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoadBalancerDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbdLoadBalancerName' - The name of the LoadBalancer.
--
-- * 'lbdDomain' - The domain name of the LoadBalancer.
--
-- * 'lbdListeners' - A list of Listeners used by the LoadBalancer.
loadBalancerDescription ::
  LoadBalancerDescription
loadBalancerDescription =
  LoadBalancerDescription'
    { _lbdLoadBalancerName = Nothing,
      _lbdDomain = Nothing,
      _lbdListeners = Nothing
    }

-- | The name of the LoadBalancer.
lbdLoadBalancerName :: Lens' LoadBalancerDescription (Maybe Text)
lbdLoadBalancerName = lens _lbdLoadBalancerName (\s a -> s {_lbdLoadBalancerName = a})

-- | The domain name of the LoadBalancer.
lbdDomain :: Lens' LoadBalancerDescription (Maybe Text)
lbdDomain = lens _lbdDomain (\s a -> s {_lbdDomain = a})

-- | A list of Listeners used by the LoadBalancer.
lbdListeners :: Lens' LoadBalancerDescription [Listener]
lbdListeners = lens _lbdListeners (\s a -> s {_lbdListeners = a}) . _Default . _Coerce

instance FromXML LoadBalancerDescription where
  parseXML x =
    LoadBalancerDescription'
      <$> (x .@? "LoadBalancerName")
      <*> (x .@? "Domain")
      <*> (x .@? "Listeners" .!@ mempty >>= may (parseXMLList "member"))

instance Hashable LoadBalancerDescription

instance NFData LoadBalancerDescription
