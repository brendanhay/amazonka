{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorPortRangeRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorPortRangeRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the Traffic Mirror filter rule port range.
--
--
--
-- /See:/ 'trafficMirrorPortRangeRequest' smart constructor.
data TrafficMirrorPortRangeRequest = TrafficMirrorPortRangeRequest'
  { _tmprrFromPort ::
      !(Maybe Int),
    _tmprrToPort :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrafficMirrorPortRangeRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tmprrFromPort' - The first port in the Traffic Mirror port range. This applies to the TCP and UDP protocols.
--
-- * 'tmprrToPort' - The last port in the Traffic Mirror port range. This applies to the TCP and UDP protocols.
trafficMirrorPortRangeRequest ::
  TrafficMirrorPortRangeRequest
trafficMirrorPortRangeRequest =
  TrafficMirrorPortRangeRequest'
    { _tmprrFromPort = Nothing,
      _tmprrToPort = Nothing
    }

-- | The first port in the Traffic Mirror port range. This applies to the TCP and UDP protocols.
tmprrFromPort :: Lens' TrafficMirrorPortRangeRequest (Maybe Int)
tmprrFromPort = lens _tmprrFromPort (\s a -> s {_tmprrFromPort = a})

-- | The last port in the Traffic Mirror port range. This applies to the TCP and UDP protocols.
tmprrToPort :: Lens' TrafficMirrorPortRangeRequest (Maybe Int)
tmprrToPort = lens _tmprrToPort (\s a -> s {_tmprrToPort = a})

instance Hashable TrafficMirrorPortRangeRequest

instance NFData TrafficMirrorPortRangeRequest

instance ToQuery TrafficMirrorPortRangeRequest where
  toQuery TrafficMirrorPortRangeRequest' {..} =
    mconcat ["FromPort" =: _tmprrFromPort, "ToPort" =: _tmprrToPort]
