{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.DescribeVirtualInterfaces
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Displays all virtual interfaces for an AWS account. Virtual interfaces
-- deleted fewer than 15 minutes before DescribeVirtualInterfaces is called
-- are also returned. If a connection ID is included then only virtual
-- interfaces associated with this connection will be returned. If a virtual
-- interface ID is included then only a single virtual interface will be
-- returned. A virtual interface (VLAN) transmits the traffic between the AWS
-- Direct Connect location and the customer. If a connection ID is provided,
-- only virtual interfaces provisioned on the specified connection will be
-- returned. If a virtual interface ID is provided, only this particular
-- virtual interface will be returned.
module Network.AWS.DirectConnect.V2012_10_25.DescribeVirtualInterfaces
    (
    -- * Request
      DescribeVirtualInterfaces
    -- ** Request constructor
    , mkDescribeVirtualInterfacesRequest
    -- ** Request lenses
    , dvitConnectionId
    , dvitVirtualInterfaceId

    -- * Response
    , DescribeVirtualInterfacesResponse
    -- ** Response lenses
    , vmVirtualInterfaces
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeVirtualInterfaces' request.
mkDescribeVirtualInterfacesRequest :: DescribeVirtualInterfaces
mkDescribeVirtualInterfacesRequest = DescribeVirtualInterfaces
    { _dvitConnectionId = Nothing
    , _dvitVirtualInterfaceId = Nothing
    }
{-# INLINE mkDescribeVirtualInterfacesRequest #-}

data DescribeVirtualInterfaces = DescribeVirtualInterfaces
    { _dvitConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _dvitVirtualInterfaceId :: Maybe Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default:
      -- None.
    } deriving (Show, Generic)

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
dvitConnectionId :: Lens' DescribeVirtualInterfaces (Maybe Text)
dvitConnectionId = lens _dvitConnectionId (\s a -> s { _dvitConnectionId = a })
{-# INLINE dvitConnectionId #-}

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
dvitVirtualInterfaceId :: Lens' DescribeVirtualInterfaces (Maybe Text)
dvitVirtualInterfaceId = lens _dvitVirtualInterfaceId (\s a -> s { _dvitVirtualInterfaceId = a })
{-# INLINE dvitVirtualInterfaceId #-}

instance ToPath DescribeVirtualInterfaces

instance ToQuery DescribeVirtualInterfaces

instance ToHeaders DescribeVirtualInterfaces

instance ToJSON DescribeVirtualInterfaces

newtype DescribeVirtualInterfacesResponse = DescribeVirtualInterfacesResponse
    { _vmVirtualInterfaces :: [VirtualInterface]
      -- ^ A list of virtual interfaces.
    } deriving (Show, Generic)

-- | A list of virtual interfaces.
vmVirtualInterfaces :: Lens' DescribeVirtualInterfacesResponse ([VirtualInterface])
vmVirtualInterfaces = lens _vmVirtualInterfaces (\s a -> s { _vmVirtualInterfaces = a })
{-# INLINE vmVirtualInterfaces #-}

instance FromJSON DescribeVirtualInterfacesResponse

instance AWSRequest DescribeVirtualInterfaces where
    type Sv DescribeVirtualInterfaces = DirectConnect
    type Rs DescribeVirtualInterfaces = DescribeVirtualInterfacesResponse

    request = get
    response _ = jsonResponse
