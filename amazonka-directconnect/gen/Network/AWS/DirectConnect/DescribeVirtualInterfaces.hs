{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeVirtualInterfaces
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Displays all virtual interfaces for an AWS account. Virtual interfaces
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
    , dvirqConnectionId
    , dvirqVirtualInterfaceId

    -- * Response
    , DescribeVirtualInterfacesResponse
    -- ** Response constructor
    , describeVirtualInterfacesResponse
    -- ** Response lenses
    , dvirsVirtualInterfaces
    , dvirsStatus
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the DescribeVirtualInterfaces operation.
--
-- /See:/ 'describeVirtualInterfaces' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvirqConnectionId'
--
-- * 'dvirqVirtualInterfaceId'
data DescribeVirtualInterfaces = DescribeVirtualInterfaces'
    { _dvirqConnectionId       :: !(Maybe Text)
    , _dvirqVirtualInterfaceId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVirtualInterfaces' smart constructor.
describeVirtualInterfaces :: DescribeVirtualInterfaces
describeVirtualInterfaces =
    DescribeVirtualInterfaces'
    { _dvirqConnectionId = Nothing
    , _dvirqVirtualInterfaceId = Nothing
    }

-- | FIXME: Undocumented member.
dvirqConnectionId :: Lens' DescribeVirtualInterfaces (Maybe Text)
dvirqConnectionId = lens _dvirqConnectionId (\ s a -> s{_dvirqConnectionId = a});

-- | FIXME: Undocumented member.
dvirqVirtualInterfaceId :: Lens' DescribeVirtualInterfaces (Maybe Text)
dvirqVirtualInterfaceId = lens _dvirqVirtualInterfaceId (\ s a -> s{_dvirqVirtualInterfaceId = a});

instance AWSRequest DescribeVirtualInterfaces where
        type Sv DescribeVirtualInterfaces = DirectConnect
        type Rs DescribeVirtualInterfaces =
             DescribeVirtualInterfacesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeVirtualInterfacesResponse' <$>
                   (x .?> "virtualInterfaces" .!@ mempty) <*>
                     (pure (fromEnum s)))

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
              ["connectionId" .= _dvirqConnectionId,
               "virtualInterfaceId" .= _dvirqVirtualInterfaceId]

instance ToPath DescribeVirtualInterfaces where
        toPath = const "/"

instance ToQuery DescribeVirtualInterfaces where
        toQuery = const mempty

-- | A structure containing a list of virtual interfaces.
--
-- /See:/ 'describeVirtualInterfacesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvirsVirtualInterfaces'
--
-- * 'dvirsStatus'
data DescribeVirtualInterfacesResponse = DescribeVirtualInterfacesResponse'
    { _dvirsVirtualInterfaces :: !(Maybe [VirtualInterface])
    , _dvirsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVirtualInterfacesResponse' smart constructor.
describeVirtualInterfacesResponse :: Int -> DescribeVirtualInterfacesResponse
describeVirtualInterfacesResponse pStatus =
    DescribeVirtualInterfacesResponse'
    { _dvirsVirtualInterfaces = Nothing
    , _dvirsStatus = pStatus
    }

-- | A list of virtual interfaces.
dvirsVirtualInterfaces :: Lens' DescribeVirtualInterfacesResponse [VirtualInterface]
dvirsVirtualInterfaces = lens _dvirsVirtualInterfaces (\ s a -> s{_dvirsVirtualInterfaces = a}) . _Default;

-- | FIXME: Undocumented member.
dvirsStatus :: Lens' DescribeVirtualInterfacesResponse Int
dvirsStatus = lens _dvirsStatus (\ s a -> s{_dvirsStatus = a});
