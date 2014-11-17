{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.DescribeVirtualInterfaces
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
--
-- <DescribeVirtualInterfaces.html>
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
    , dvirVirtualInterfaces
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.Types
import qualified GHC.Exts

data DescribeVirtualInterfaces = DescribeVirtualInterfaces
    { _dviConnectionId       :: Maybe Text
    , _dviVirtualInterfaceId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeVirtualInterfaces' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dviConnectionId' @::@ 'Maybe' 'Text'
--
-- * 'dviVirtualInterfaceId' @::@ 'Maybe' 'Text'
--
describeVirtualInterfaces :: DescribeVirtualInterfaces
describeVirtualInterfaces = DescribeVirtualInterfaces
    { _dviConnectionId       = Nothing
    , _dviVirtualInterfaceId = Nothing
    }

dviConnectionId :: Lens' DescribeVirtualInterfaces (Maybe Text)
dviConnectionId = lens _dviConnectionId (\s a -> s { _dviConnectionId = a })

dviVirtualInterfaceId :: Lens' DescribeVirtualInterfaces (Maybe Text)
dviVirtualInterfaceId =
    lens _dviVirtualInterfaceId (\s a -> s { _dviVirtualInterfaceId = a })

newtype DescribeVirtualInterfacesResponse = DescribeVirtualInterfacesResponse
    { _dvirVirtualInterfaces :: [VirtualInterface]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeVirtualInterfacesResponse where
    type Item DescribeVirtualInterfacesResponse = VirtualInterface

    fromList = DescribeVirtualInterfacesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dvirVirtualInterfaces

-- | 'DescribeVirtualInterfacesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvirVirtualInterfaces' @::@ ['VirtualInterface']
--
describeVirtualInterfacesResponse :: DescribeVirtualInterfacesResponse
describeVirtualInterfacesResponse = DescribeVirtualInterfacesResponse
    { _dvirVirtualInterfaces = mempty
    }

-- | A list of virtual interfaces.
dvirVirtualInterfaces :: Lens' DescribeVirtualInterfacesResponse [VirtualInterface]
dvirVirtualInterfaces =
    lens _dvirVirtualInterfaces (\s a -> s { _dvirVirtualInterfaces = a })

instance AWSRequest DescribeVirtualInterfaces where
    type Sv DescribeVirtualInterfaces = DirectConnect
    type Rs DescribeVirtualInterfaces = DescribeVirtualInterfacesResponse

    request  = post
    response = jsonResponse

instance FromJSON DescribeVirtualInterfacesResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath DescribeVirtualInterfaces where
    toPath = const "/"

instance ToHeaders DescribeVirtualInterfaces

instance ToQuery DescribeVirtualInterfaces where
    toQuery = const mempty

instance ToJSON DescribeVirtualInterfaces where
    toJSON = genericToJSON jsonOptions
