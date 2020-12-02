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
-- Module      : Network.AWS.EC2.DescribeNetworkInterfacePermissions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions for your network interfaces.
--
--
module Network.AWS.EC2.DescribeNetworkInterfacePermissions
    (
    -- * Creating a Request
      describeNetworkInterfacePermissions
    , DescribeNetworkInterfacePermissions
    -- * Request Lenses
    , dnipFilters
    , dnipNextToken
    , dnipNetworkInterfacePermissionIds
    , dnipMaxResults

    -- * Destructuring the Response
    , describeNetworkInterfacePermissionsResponse
    , DescribeNetworkInterfacePermissionsResponse
    -- * Response Lenses
    , dnipsrsNetworkInterfacePermissions
    , dnipsrsNextToken
    , dnipsrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeNetworkInterfacePermissions.
--
--
--
-- /See:/ 'describeNetworkInterfacePermissions' smart constructor.
data DescribeNetworkInterfacePermissions = DescribeNetworkInterfacePermissions'
  { _dnipFilters                       :: !(Maybe [Filter])
  , _dnipNextToken                     :: !(Maybe Text)
  , _dnipNetworkInterfacePermissionIds :: !(Maybe [Text])
  , _dnipMaxResults                    :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNetworkInterfacePermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnipFilters' - One or more filters.     * @network-interface-permission.network-interface-permission-id@ - The ID of the permission.     * @network-interface-permission.network-interface-id@ - The ID of the network interface.     * @network-interface-permission.aws-account-id@ - The AWS account ID.     * @network-interface-permission.aws-service@ - The AWS service.     * @network-interface-permission.permission@ - The type of permission (@INSTANCE-ATTACH@ | @EIP-ASSOCIATE@ ).
--
-- * 'dnipNextToken' - The token to request the next page of results.
--
-- * 'dnipNetworkInterfacePermissionIds' - One or more network interface permission IDs.
--
-- * 'dnipMaxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. If this parameter is not specified, up to 50 results are returned by default.
describeNetworkInterfacePermissions
    :: DescribeNetworkInterfacePermissions
describeNetworkInterfacePermissions =
  DescribeNetworkInterfacePermissions'
    { _dnipFilters = Nothing
    , _dnipNextToken = Nothing
    , _dnipNetworkInterfacePermissionIds = Nothing
    , _dnipMaxResults = Nothing
    }


-- | One or more filters.     * @network-interface-permission.network-interface-permission-id@ - The ID of the permission.     * @network-interface-permission.network-interface-id@ - The ID of the network interface.     * @network-interface-permission.aws-account-id@ - The AWS account ID.     * @network-interface-permission.aws-service@ - The AWS service.     * @network-interface-permission.permission@ - The type of permission (@INSTANCE-ATTACH@ | @EIP-ASSOCIATE@ ).
dnipFilters :: Lens' DescribeNetworkInterfacePermissions [Filter]
dnipFilters = lens _dnipFilters (\ s a -> s{_dnipFilters = a}) . _Default . _Coerce

-- | The token to request the next page of results.
dnipNextToken :: Lens' DescribeNetworkInterfacePermissions (Maybe Text)
dnipNextToken = lens _dnipNextToken (\ s a -> s{_dnipNextToken = a})

-- | One or more network interface permission IDs.
dnipNetworkInterfacePermissionIds :: Lens' DescribeNetworkInterfacePermissions [Text]
dnipNetworkInterfacePermissionIds = lens _dnipNetworkInterfacePermissionIds (\ s a -> s{_dnipNetworkInterfacePermissionIds = a}) . _Default . _Coerce

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. If this parameter is not specified, up to 50 results are returned by default.
dnipMaxResults :: Lens' DescribeNetworkInterfacePermissions (Maybe Int)
dnipMaxResults = lens _dnipMaxResults (\ s a -> s{_dnipMaxResults = a})

instance AWSRequest
           DescribeNetworkInterfacePermissions
         where
        type Rs DescribeNetworkInterfacePermissions =
             DescribeNetworkInterfacePermissionsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeNetworkInterfacePermissionsResponse' <$>
                   (x .@? "networkInterfacePermissions" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeNetworkInterfacePermissions
         where

instance NFData DescribeNetworkInterfacePermissions
         where

instance ToHeaders
           DescribeNetworkInterfacePermissions
         where
        toHeaders = const mempty

instance ToPath DescribeNetworkInterfacePermissions
         where
        toPath = const "/"

instance ToQuery DescribeNetworkInterfacePermissions
         where
        toQuery DescribeNetworkInterfacePermissions'{..}
          = mconcat
              ["Action" =:
                 ("DescribeNetworkInterfacePermissions" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dnipFilters),
               "NextToken" =: _dnipNextToken,
               toQuery
                 (toQueryList "NetworkInterfacePermissionId" <$>
                    _dnipNetworkInterfacePermissionIds),
               "MaxResults" =: _dnipMaxResults]

-- | Contains the output for DescribeNetworkInterfacePermissions.
--
--
--
-- /See:/ 'describeNetworkInterfacePermissionsResponse' smart constructor.
data DescribeNetworkInterfacePermissionsResponse = DescribeNetworkInterfacePermissionsResponse'
  { _dnipsrsNetworkInterfacePermissions :: !(Maybe [NetworkInterfacePermission])
  , _dnipsrsNextToken                   :: !(Maybe Text)
  , _dnipsrsResponseStatus              :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNetworkInterfacePermissionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnipsrsNetworkInterfacePermissions' - The network interface permissions.
--
-- * 'dnipsrsNextToken' - The token to use to retrieve the next page of results.
--
-- * 'dnipsrsResponseStatus' - -- | The response status code.
describeNetworkInterfacePermissionsResponse
    :: Int -- ^ 'dnipsrsResponseStatus'
    -> DescribeNetworkInterfacePermissionsResponse
describeNetworkInterfacePermissionsResponse pResponseStatus_ =
  DescribeNetworkInterfacePermissionsResponse'
    { _dnipsrsNetworkInterfacePermissions = Nothing
    , _dnipsrsNextToken = Nothing
    , _dnipsrsResponseStatus = pResponseStatus_
    }


-- | The network interface permissions.
dnipsrsNetworkInterfacePermissions :: Lens' DescribeNetworkInterfacePermissionsResponse [NetworkInterfacePermission]
dnipsrsNetworkInterfacePermissions = lens _dnipsrsNetworkInterfacePermissions (\ s a -> s{_dnipsrsNetworkInterfacePermissions = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results.
dnipsrsNextToken :: Lens' DescribeNetworkInterfacePermissionsResponse (Maybe Text)
dnipsrsNextToken = lens _dnipsrsNextToken (\ s a -> s{_dnipsrsNextToken = a})

-- | -- | The response status code.
dnipsrsResponseStatus :: Lens' DescribeNetworkInterfacePermissionsResponse Int
dnipsrsResponseStatus = lens _dnipsrsResponseStatus (\ s a -> s{_dnipsrsResponseStatus = a})

instance NFData
           DescribeNetworkInterfacePermissionsResponse
         where
