{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorPortRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorPortRange where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Traffic Mirror port range.
--
--
--
-- /See:/ 'trafficMirrorPortRange' smart constructor.
data TrafficMirrorPortRange = TrafficMirrorPortRange'
  { _tmprFromPort ::
      !(Maybe Int),
    _tmprToPort :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrafficMirrorPortRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tmprFromPort' - The start of the Traffic Mirror port range. This applies to the TCP and UDP protocols.
--
-- * 'tmprToPort' - The end of the Traffic Mirror port range. This applies to the TCP and UDP protocols.
trafficMirrorPortRange ::
  TrafficMirrorPortRange
trafficMirrorPortRange =
  TrafficMirrorPortRange'
    { _tmprFromPort = Nothing,
      _tmprToPort = Nothing
    }

-- | The start of the Traffic Mirror port range. This applies to the TCP and UDP protocols.
tmprFromPort :: Lens' TrafficMirrorPortRange (Maybe Int)
tmprFromPort = lens _tmprFromPort (\s a -> s {_tmprFromPort = a})

-- | The end of the Traffic Mirror port range. This applies to the TCP and UDP protocols.
tmprToPort :: Lens' TrafficMirrorPortRange (Maybe Int)
tmprToPort = lens _tmprToPort (\s a -> s {_tmprToPort = a})

instance FromXML TrafficMirrorPortRange where
  parseXML x =
    TrafficMirrorPortRange'
      <$> (x .@? "fromPort") <*> (x .@? "toPort")

instance Hashable TrafficMirrorPortRange

instance NFData TrafficMirrorPortRange
