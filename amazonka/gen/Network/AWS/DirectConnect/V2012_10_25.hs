-- Module      : Network.AWS.DirectConnect.V2012_10_25
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Direct Connect links your internal network to an AWS Direct Connect
-- location over a standard 1 gigabit or 10 gigabit Ethernet fiber-optic
-- cable. One end of the cable is connected to your router, the other to an
-- AWS Direct Connect router. With this connection in place, you can create
-- virtual interfaces directly to the AWS cloud and Amazon Virtual Private
-- Cloud, bypassing Internet service providers in your network path.
module Network.AWS.DirectConnect.V2012_10_25 (module Export) where

import Network.AWS.DirectConnect.V2012_10_25.AllocateConnectionOnInterconnect as Export
import Network.AWS.DirectConnect.V2012_10_25.AllocatePrivateVirtualInterface as Export
import Network.AWS.DirectConnect.V2012_10_25.AllocatePublicVirtualInterface as Export
import Network.AWS.DirectConnect.V2012_10_25.ConfirmConnection as Export
import Network.AWS.DirectConnect.V2012_10_25.ConfirmPrivateVirtualInterface as Export
import Network.AWS.DirectConnect.V2012_10_25.ConfirmPublicVirtualInterface as Export
import Network.AWS.DirectConnect.V2012_10_25.CreateConnection as Export
import Network.AWS.DirectConnect.V2012_10_25.CreateInterconnect as Export
import Network.AWS.DirectConnect.V2012_10_25.CreatePrivateVirtualInterface as Export
import Network.AWS.DirectConnect.V2012_10_25.CreatePublicVirtualInterface as Export
import Network.AWS.DirectConnect.V2012_10_25.DeleteConnection as Export
import Network.AWS.DirectConnect.V2012_10_25.DeleteInterconnect as Export
import Network.AWS.DirectConnect.V2012_10_25.DeleteVirtualInterface as Export
import Network.AWS.DirectConnect.V2012_10_25.DescribeConnections as Export
import Network.AWS.DirectConnect.V2012_10_25.DescribeConnectionsOnInterconnect as Export
import Network.AWS.DirectConnect.V2012_10_25.DescribeInterconnects as Export
import Network.AWS.DirectConnect.V2012_10_25.DescribeLocations as Export
import Network.AWS.DirectConnect.V2012_10_25.DescribeVirtualGateways as Export
import Network.AWS.DirectConnect.V2012_10_25.DescribeVirtualInterfaces as Export
import Network.AWS.DirectConnect.V2012_10_25.Types as Export
