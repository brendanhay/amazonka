{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.DirectConnect.V2012_10_25.DescribeVirtualInterfaces where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.V2012_10_25.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeVirtualInterfaces' request.
describeVirtualInterfaces :: DescribeVirtualInterfaces
describeVirtualInterfaces = DescribeVirtualInterfaces
    { _dvirConnectionId = Nothing
    , _dvirVirtualInterfaceId = Nothing
    }

data DescribeVirtualInterfaces = DescribeVirtualInterfaces
    { _dvirConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _dvirVirtualInterfaceId :: Maybe Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default:
      -- None.
    } deriving (Generic)

makeLenses ''DescribeVirtualInterfaces

instance ToPath DescribeVirtualInterfaces

instance ToQuery DescribeVirtualInterfaces

instance ToHeaders DescribeVirtualInterfaces

instance ToJSON DescribeVirtualInterfaces

data DescribeVirtualInterfacesResponse = DescribeVirtualInterfacesResponse
    { _vkVirtualInterfaces :: [VirtualInterface]
      -- ^ A list of virtual interfaces.
    } deriving (Generic)

makeLenses ''DescribeVirtualInterfacesResponse

instance FromJSON DescribeVirtualInterfacesResponse

instance AWSRequest DescribeVirtualInterfaces where
    type Sv DescribeVirtualInterfaces = DirectConnect
    type Rs DescribeVirtualInterfaces = DescribeVirtualInterfacesResponse

    request = get
    response _ = jsonResponse
