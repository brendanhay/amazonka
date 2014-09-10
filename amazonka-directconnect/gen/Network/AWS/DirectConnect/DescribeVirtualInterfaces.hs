{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect
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
module Network.AWS.DirectConnect
    (
    -- * Request
      DescribeVirtualInterfaces
    -- ** Request constructor
    , mkDescribeVirtualInterfaces
    -- ** Request lenses
    , dvi1ConnectionId
    , dvi1VirtualInterfaceId

    -- * Response
    , DescribeVirtualInterfacesResponse
    -- ** Response constructor
    , mkDescribeVirtualInterfacesResponse
    -- ** Response lenses
    , dvirrVirtualInterfaces
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Container for the parameters to the DescribeVirtualInterfaces operation.
data DescribeVirtualInterfaces = DescribeVirtualInterfaces
    { _dvi1ConnectionId :: !(Maybe Text)
    , _dvi1VirtualInterfaceId :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeVirtualInterfaces' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ConnectionId ::@ @Maybe Text@
--
-- * @VirtualInterfaceId ::@ @Maybe Text@
--
mkDescribeVirtualInterfaces :: DescribeVirtualInterfaces
mkDescribeVirtualInterfaces = DescribeVirtualInterfaces
    { _dvi1ConnectionId = Nothing
    , _dvi1VirtualInterfaceId = Nothing
    }

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
dvi1ConnectionId :: Lens' DescribeVirtualInterfaces (Maybe Text)
dvi1ConnectionId =
    lens _dvi1ConnectionId (\s a -> s { _dvi1ConnectionId = a })

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
dvi1VirtualInterfaceId :: Lens' DescribeVirtualInterfaces (Maybe Text)
dvi1VirtualInterfaceId =
    lens _dvi1VirtualInterfaceId (\s a -> s { _dvi1VirtualInterfaceId = a })

instance ToPath DescribeVirtualInterfaces

instance ToQuery DescribeVirtualInterfaces

instance ToHeaders DescribeVirtualInterfaces

instance ToJSON DescribeVirtualInterfaces

-- | A structure containing a list of virtual interfaces.
newtype DescribeVirtualInterfacesResponse = DescribeVirtualInterfacesResponse
    { _dvirrVirtualInterfaces :: [VirtualInterface]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeVirtualInterfacesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VirtualInterfaces ::@ @[VirtualInterface]@
--
mkDescribeVirtualInterfacesResponse :: DescribeVirtualInterfacesResponse
mkDescribeVirtualInterfacesResponse = DescribeVirtualInterfacesResponse
    { _dvirrVirtualInterfaces = mempty
    }

-- | A list of virtual interfaces.
dvirrVirtualInterfaces :: Lens' DescribeVirtualInterfacesResponse [VirtualInterface]
dvirrVirtualInterfaces =
    lens _dvirrVirtualInterfaces (\s a -> s { _dvirrVirtualInterfaces = a })

instance FromJSON DescribeVirtualInterfacesResponse

instance AWSRequest DescribeVirtualInterfaces where
    type Sv DescribeVirtualInterfaces = DirectConnect
    type Rs DescribeVirtualInterfaces = DescribeVirtualInterfacesResponse

    request = get
    response _ = jsonResponse
