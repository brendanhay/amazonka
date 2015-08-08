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
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeVirtualInterfaces.html AWS API Reference> for DescribeVirtualInterfaces.
module Network.AWS.DirectConnect.DescribeVirtualInterfaces
    (
    -- * Creating a Request
      DescribeVirtualInterfaces
    , describeVirtualInterfaces
    -- * Request Lenses
    , dviConnectionId
    , dviVirtualInterfaceId

    -- * Destructuring the Response
    , DescribeVirtualInterfacesResponse
    , describeVirtualInterfacesResponse
    -- * Response Lenses
    , dvisrsVirtualInterfaces
    , dvisrsStatus
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
-- * 'dviConnectionId'
--
-- * 'dviVirtualInterfaceId'
data DescribeVirtualInterfaces = DescribeVirtualInterfaces'
    { _dviConnectionId       :: !(Maybe Text)
    , _dviVirtualInterfaceId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVirtualInterfaces' smart constructor.
describeVirtualInterfaces :: DescribeVirtualInterfaces
describeVirtualInterfaces =
    DescribeVirtualInterfaces'
    { _dviConnectionId = Nothing
    , _dviVirtualInterfaceId = Nothing
    }

-- | Undocumented member.
dviConnectionId :: Lens' DescribeVirtualInterfaces (Maybe Text)
dviConnectionId = lens _dviConnectionId (\ s a -> s{_dviConnectionId = a});

-- | Undocumented member.
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
              ["connectionId" .= _dviConnectionId,
               "virtualInterfaceId" .= _dviVirtualInterfaceId]

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
-- * 'dvisrsVirtualInterfaces'
--
-- * 'dvisrsStatus'
data DescribeVirtualInterfacesResponse = DescribeVirtualInterfacesResponse'
    { _dvisrsVirtualInterfaces :: !(Maybe [VirtualInterface])
    , _dvisrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVirtualInterfacesResponse' smart constructor.
describeVirtualInterfacesResponse :: Int -> DescribeVirtualInterfacesResponse
describeVirtualInterfacesResponse pStatus_ =
    DescribeVirtualInterfacesResponse'
    { _dvisrsVirtualInterfaces = Nothing
    , _dvisrsStatus = pStatus_
    }

-- | A list of virtual interfaces.
dvisrsVirtualInterfaces :: Lens' DescribeVirtualInterfacesResponse [VirtualInterface]
dvisrsVirtualInterfaces = lens _dvisrsVirtualInterfaces (\ s a -> s{_dvisrsVirtualInterfaces = a}) . _Default . _Coerce;

-- | Undocumented member.
dvisrsStatus :: Lens' DescribeVirtualInterfacesResponse Int
dvisrsStatus = lens _dvisrsStatus (\ s a -> s{_dvisrsStatus = a});
