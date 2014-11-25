-- Module      : Network.AWS.DirectConnect
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Direct Connect links your internal network to an AWS Direct Connect
-- location over a standard 1 gigabit or 10 gigabit Ethernet fiber-optic cable.
-- One end of the cable is connected to your router, the other to an AWS Direct
-- Connect router. With this connection in place, you can create virtual
-- interfaces directly to the AWS cloud and Amazon Virtual Private Cloud,
-- bypassing Internet service providers in your network path.
module Network.AWS.DirectConnect
    ( module Network.AWS.DirectConnect.AllocateConnectionOnInterconnect
    , module Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
    , module Network.AWS.DirectConnect.AllocatePublicVirtualInterface
    , module Network.AWS.DirectConnect.ConfirmConnection
    , module Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
    , module Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
    , module Network.AWS.DirectConnect.CreateConnection
    , module Network.AWS.DirectConnect.CreateInterconnect
    , module Network.AWS.DirectConnect.CreatePrivateVirtualInterface
    , module Network.AWS.DirectConnect.CreatePublicVirtualInterface
    , module Network.AWS.DirectConnect.DeleteConnection
    , module Network.AWS.DirectConnect.DeleteInterconnect
    , module Network.AWS.DirectConnect.DeleteVirtualInterface
    , module Network.AWS.DirectConnect.DescribeConnections
    , module Network.AWS.DirectConnect.DescribeConnectionsOnInterconnect
    , module Network.AWS.DirectConnect.DescribeInterconnects
    , module Network.AWS.DirectConnect.DescribeLocations
    , module Network.AWS.DirectConnect.DescribeVirtualGateways
    , module Network.AWS.DirectConnect.DescribeVirtualInterfaces
    , module Network.AWS.DirectConnect.Types
    ) where

import Network.AWS.DirectConnect.AllocateConnectionOnInterconnect
import Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
import Network.AWS.DirectConnect.AllocatePublicVirtualInterface
import Network.AWS.DirectConnect.ConfirmConnection
import Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
import Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
import Network.AWS.DirectConnect.CreateConnection
import Network.AWS.DirectConnect.CreateInterconnect
import Network.AWS.DirectConnect.CreatePrivateVirtualInterface
import Network.AWS.DirectConnect.CreatePublicVirtualInterface
import Network.AWS.DirectConnect.DeleteConnection
import Network.AWS.DirectConnect.DeleteInterconnect
import Network.AWS.DirectConnect.DeleteVirtualInterface
import Network.AWS.DirectConnect.DescribeConnections
import Network.AWS.DirectConnect.DescribeConnectionsOnInterconnect
import Network.AWS.DirectConnect.DescribeInterconnects
import Network.AWS.DirectConnect.DescribeLocations
import Network.AWS.DirectConnect.DescribeVirtualGateways
import Network.AWS.DirectConnect.DescribeVirtualInterfaces
import Network.AWS.DirectConnect.Types
