{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TrafficRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TrafficRoute where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a listener. The listener contains the path used to route traffic that is received from the load balancer to a target group.
--
--
--
-- /See:/ 'trafficRoute' smart constructor.
newtype TrafficRoute = TrafficRoute'
  { _trListenerARNs ::
      Maybe [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrafficRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trListenerARNs' - The Amazon Resource Name (ARN) of one listener. The listener identifies the route between a target group and a load balancer. This is an array of strings with a maximum size of one.
trafficRoute ::
  TrafficRoute
trafficRoute = TrafficRoute' {_trListenerARNs = Nothing}

-- | The Amazon Resource Name (ARN) of one listener. The listener identifies the route between a target group and a load balancer. This is an array of strings with a maximum size of one.
trListenerARNs :: Lens' TrafficRoute [Text]
trListenerARNs = lens _trListenerARNs (\s a -> s {_trListenerARNs = a}) . _Default . _Coerce

instance FromJSON TrafficRoute where
  parseJSON =
    withObject
      "TrafficRoute"
      (\x -> TrafficRoute' <$> (x .:? "listenerArns" .!= mempty))

instance Hashable TrafficRoute

instance NFData TrafficRoute

instance ToJSON TrafficRoute where
  toJSON TrafficRoute' {..} =
    object (catMaybes [("listenerArns" .=) <$> _trListenerARNs])
