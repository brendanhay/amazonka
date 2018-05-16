{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeVPCPeeringAuthorizations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves valid VPC peering authorizations that are pending for the AWS account. This operation returns all VPC peering authorizations and requests for peering. This includes those initiated and received by this account.
--
--
-- VPC peering connection operations include:
--
--     * 'CreateVpcPeeringAuthorization'
--
--     * 'DescribeVpcPeeringAuthorizations'
--
--     * 'DeleteVpcPeeringAuthorization'
--
--     * 'CreateVpcPeeringConnection'
--
--     * 'DescribeVpcPeeringConnections'
--
--     * 'DeleteVpcPeeringConnection'
--
--
--
module Network.AWS.GameLift.DescribeVPCPeeringAuthorizations
    (
    -- * Creating a Request
      describeVPCPeeringAuthorizations
    , DescribeVPCPeeringAuthorizations

    -- * Destructuring the Response
    , describeVPCPeeringAuthorizationsResponse
    , DescribeVPCPeeringAuthorizationsResponse
    -- * Response Lenses
    , dvpcparsVPCPeeringAuthorizations
    , dvpcparsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeVPCPeeringAuthorizations' smart constructor.
data DescribeVPCPeeringAuthorizations =
  DescribeVPCPeeringAuthorizations'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCPeeringAuthorizations' with the minimum fields required to make a request.
--
describeVPCPeeringAuthorizations
    :: DescribeVPCPeeringAuthorizations
describeVPCPeeringAuthorizations = DescribeVPCPeeringAuthorizations'


instance AWSRequest DescribeVPCPeeringAuthorizations
         where
        type Rs DescribeVPCPeeringAuthorizations =
             DescribeVPCPeeringAuthorizationsResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribeVPCPeeringAuthorizationsResponse' <$>
                   (x .?> "VpcPeeringAuthorizations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeVPCPeeringAuthorizations
         where

instance NFData DescribeVPCPeeringAuthorizations
         where

instance ToHeaders DescribeVPCPeeringAuthorizations
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribeVpcPeeringAuthorizations" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeVPCPeeringAuthorizations
         where
        toJSON = const (Object mempty)

instance ToPath DescribeVPCPeeringAuthorizations
         where
        toPath = const "/"

instance ToQuery DescribeVPCPeeringAuthorizations
         where
        toQuery = const mempty

-- | /See:/ 'describeVPCPeeringAuthorizationsResponse' smart constructor.
data DescribeVPCPeeringAuthorizationsResponse = DescribeVPCPeeringAuthorizationsResponse'
  { _dvpcparsVPCPeeringAuthorizations :: !(Maybe [VPCPeeringAuthorization])
  , _dvpcparsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCPeeringAuthorizationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpcparsVPCPeeringAuthorizations' - Collection of objects that describe all valid VPC peering operations for the current AWS account.
--
-- * 'dvpcparsResponseStatus' - -- | The response status code.
describeVPCPeeringAuthorizationsResponse
    :: Int -- ^ 'dvpcparsResponseStatus'
    -> DescribeVPCPeeringAuthorizationsResponse
describeVPCPeeringAuthorizationsResponse pResponseStatus_ =
  DescribeVPCPeeringAuthorizationsResponse'
    { _dvpcparsVPCPeeringAuthorizations = Nothing
    , _dvpcparsResponseStatus = pResponseStatus_
    }


-- | Collection of objects that describe all valid VPC peering operations for the current AWS account.
dvpcparsVPCPeeringAuthorizations :: Lens' DescribeVPCPeeringAuthorizationsResponse [VPCPeeringAuthorization]
dvpcparsVPCPeeringAuthorizations = lens _dvpcparsVPCPeeringAuthorizations (\ s a -> s{_dvpcparsVPCPeeringAuthorizations = a}) . _Default . _Coerce

-- | -- | The response status code.
dvpcparsResponseStatus :: Lens' DescribeVPCPeeringAuthorizationsResponse Int
dvpcparsResponseStatus = lens _dvpcparsResponseStatus (\ s a -> s{_dvpcparsResponseStatus = a})

instance NFData
           DescribeVPCPeeringAuthorizationsResponse
         where
