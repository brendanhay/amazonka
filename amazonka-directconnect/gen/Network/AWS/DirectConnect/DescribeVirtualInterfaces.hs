{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DirectConnect.DescribeVirtualInterfaces
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

-- | Displays all virtual interfaces for an AWS account. Virtual interfaces
-- deleted fewer than 15 minutes before DescribeVirtualInterfaces is called
-- are also returned. If a connection ID is included then only virtual
-- interfaces associated with this connection will be returned. If a
-- virtual interface ID is included then only a single virtual interface
-- will be returned.
--
-- A virtual interface (VLAN) transmits the traffic between the AWS Direct
-- Connect location and the customer.
--
-- If a connection ID is provided, only virtual interfaces provisioned on
-- the specified connection will be returned. If a virtual interface ID is
-- provided, only this particular virtual interface will be returned.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeVirtualInterfaces.html>
module Network.AWS.DirectConnect.DescribeVirtualInterfaces
    (
    -- * Request
      DescribeVirtualInterfaces
    -- ** Request constructor
    , describeVirtualInterfaces
    -- ** Request lenses
    , dviConnectionId
    , dviVirtualInterfaceId

    -- * Response
    , DescribeVirtualInterfacesResponse
    -- ** Response constructor
    , describeVirtualInterfacesResponse
    -- ** Response lenses
    , dvirDescribeVirtualInterfacesResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.DirectConnect.Types

-- | /See:/ 'describeVirtualInterfaces' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dviConnectionId'
--
-- * 'dviVirtualInterfaceId'
data DescribeVirtualInterfaces = DescribeVirtualInterfaces'{_dviConnectionId :: Maybe Text, _dviVirtualInterfaceId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeVirtualInterfaces' smart constructor.
describeVirtualInterfaces :: DescribeVirtualInterfaces
describeVirtualInterfaces = DescribeVirtualInterfaces'{_dviConnectionId = Nothing, _dviVirtualInterfaceId = Nothing};

-- | FIXME: Undocumented member.
dviConnectionId :: Lens' DescribeVirtualInterfaces (Maybe Text)
dviConnectionId = lens _dviConnectionId (\ s a -> s{_dviConnectionId = a});

-- | FIXME: Undocumented member.
dviVirtualInterfaceId :: Lens' DescribeVirtualInterfaces (Maybe Text)
dviVirtualInterfaceId = lens _dviVirtualInterfaceId (\ s a -> s{_dviVirtualInterfaceId = a});

instance AWSRequest DescribeVirtualInterfaces where
        type Sv DescribeVirtualInterfaces = DirectConnect
        type Rs DescribeVirtualInterfaces =
             DescribeVirtualInterfacesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeVirtualInterfacesResponse' <$>
                   x .?> "DescribeVirtualInterfacesResponse" .!@ mempty)

instance ToHeaders DescribeVirtualInterfaces where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeVirtualInterfaces" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeVirtualInterfaces where
        toJSON DescribeVirtualInterfaces'{..}
          = object
              ["connectionId" .= _dviConnectionId,
               "virtualInterfaceId" .= _dviVirtualInterfaceId]

instance ToPath DescribeVirtualInterfaces where
        toPath = const "/"

instance ToQuery DescribeVirtualInterfaces where
        toQuery = const mempty

-- | /See:/ 'describeVirtualInterfacesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvirDescribeVirtualInterfacesResponse'
newtype DescribeVirtualInterfacesResponse = DescribeVirtualInterfacesResponse'{_dvirDescribeVirtualInterfacesResponse :: [VirtualInterface]} deriving (Eq, Read, Show)

-- | 'DescribeVirtualInterfacesResponse' smart constructor.
describeVirtualInterfacesResponse :: DescribeVirtualInterfacesResponse
describeVirtualInterfacesResponse = DescribeVirtualInterfacesResponse'{_dvirDescribeVirtualInterfacesResponse = mempty};

-- | A list of virtual interfaces.
dvirDescribeVirtualInterfacesResponse :: Lens' DescribeVirtualInterfacesResponse [VirtualInterface]
dvirDescribeVirtualInterfacesResponse = lens _dvirDescribeVirtualInterfacesResponse (\ s a -> s{_dvirDescribeVirtualInterfacesResponse = a});
