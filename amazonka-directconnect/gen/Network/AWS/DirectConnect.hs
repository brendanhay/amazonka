-- Module      : Network.AWS.DirectConnect
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | AWS Direct Connect makes it easy to establish a dedicated network
-- connection from your premises to Amazon Web Services (AWS). Using AWS
-- Direct Connect, you can establish private connectivity between AWS and
-- your data center, office, or colocation environment, which in many cases
-- can reduce your network costs, increase bandwidth throughput, and
-- provide a more consistent network experience than Internet-based
-- connections.
--
-- The AWS Direct Connect API Reference provides descriptions, syntax, and
-- usage examples for each of the actions and data types for AWS Direct
-- Connect. Use the following links to get started using the /AWS Direct
-- Connect API Reference/:
--
-- -   <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_Operations.html Actions>:
--     An alphabetical list of all AWS Direct Connect actions.
-- -   <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_Types.html Data Types>:
--     An alphabetical list of all AWS Direct Connect data types.
-- -   <http://docs.aws.amazon.com/directconnect/latest/APIReference/CommonParameters.html Common Query Parameters>:
--     Parameters that all Query actions can use.
-- -   <http://docs.aws.amazon.com/directconnect/latest/APIReference/CommonErrors.html Common Errors>:
--     Client and server errors that all actions can return.
module Network.AWS.DirectConnect
    ( module Export
    ) where

import           Network.AWS.DirectConnect.AllocateConnectionOnInterconnect  as Export
import           Network.AWS.DirectConnect.AllocatePrivateVirtualInterface   as Export
import           Network.AWS.DirectConnect.AllocatePublicVirtualInterface    as Export
import           Network.AWS.DirectConnect.ConfirmConnection                 as Export
import           Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface    as Export
import           Network.AWS.DirectConnect.ConfirmPublicVirtualInterface     as Export
import           Network.AWS.DirectConnect.CreateConnection                  as Export
import           Network.AWS.DirectConnect.CreateInterconnect                as Export
import           Network.AWS.DirectConnect.CreatePrivateVirtualInterface     as Export
import           Network.AWS.DirectConnect.CreatePublicVirtualInterface      as Export
import           Network.AWS.DirectConnect.DeleteConnection                  as Export
import           Network.AWS.DirectConnect.DeleteInterconnect                as Export
import           Network.AWS.DirectConnect.DeleteVirtualInterface            as Export
import           Network.AWS.DirectConnect.DescribeConnections               as Export
import           Network.AWS.DirectConnect.DescribeConnectionsOnInterconnect as Export
import           Network.AWS.DirectConnect.DescribeInterconnects             as Export
import           Network.AWS.DirectConnect.DescribeLocations                 as Export
import           Network.AWS.DirectConnect.DescribeVirtualGateways           as Export
import           Network.AWS.DirectConnect.DescribeVirtualInterfaces         as Export
import           Network.AWS.DirectConnect.Types                             as Export
import           Network.AWS.DirectConnect.Waiters                           as Export
