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
-- Module      : Network.AWS.EC2.DescribeAggregateIdFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the longer ID format settings for all resource types in a specific region. This request is useful for performing a quick audit to determine whether a specific region is fully opted in for longer IDs (17-character IDs).
--
--
-- This request only returns information about resource types that support longer IDs.
--
-- The following resource types support longer IDs: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ .
--
module Network.AWS.EC2.DescribeAggregateIdFormat
    (
    -- * Creating a Request
      describeAggregateIdFormat
    , DescribeAggregateIdFormat
    -- * Request Lenses
    , daifDryRun

    -- * Destructuring the Response
    , describeAggregateIdFormatResponse
    , DescribeAggregateIdFormatResponse
    -- * Response Lenses
    , daifrsUseLongIdsAggregated
    , daifrsStatuses
    , daifrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAggregateIdFormat' smart constructor.
newtype DescribeAggregateIdFormat = DescribeAggregateIdFormat'
  { _daifDryRun :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAggregateIdFormat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daifDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
describeAggregateIdFormat
    :: DescribeAggregateIdFormat
describeAggregateIdFormat = DescribeAggregateIdFormat' {_daifDryRun = Nothing}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
daifDryRun :: Lens' DescribeAggregateIdFormat (Maybe Bool)
daifDryRun = lens _daifDryRun (\ s a -> s{_daifDryRun = a})

instance AWSRequest DescribeAggregateIdFormat where
        type Rs DescribeAggregateIdFormat =
             DescribeAggregateIdFormatResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeAggregateIdFormatResponse' <$>
                   (x .@? "useLongIdsAggregated") <*>
                     (x .@? "statusSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeAggregateIdFormat where

instance NFData DescribeAggregateIdFormat where

instance ToHeaders DescribeAggregateIdFormat where
        toHeaders = const mempty

instance ToPath DescribeAggregateIdFormat where
        toPath = const "/"

instance ToQuery DescribeAggregateIdFormat where
        toQuery DescribeAggregateIdFormat'{..}
          = mconcat
              ["Action" =:
                 ("DescribeAggregateIdFormat" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _daifDryRun]

-- | /See:/ 'describeAggregateIdFormatResponse' smart constructor.
data DescribeAggregateIdFormatResponse = DescribeAggregateIdFormatResponse'
  { _daifrsUseLongIdsAggregated :: !(Maybe Bool)
  , _daifrsStatuses             :: !(Maybe [IdFormat])
  , _daifrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAggregateIdFormatResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daifrsUseLongIdsAggregated' - Indicates whether all resource types in the region are configured to use longer IDs. This value is only @true@ if all users are configured to use longer IDs for all resources types in the region.
--
-- * 'daifrsStatuses' - Information about each resource's ID format.
--
-- * 'daifrsResponseStatus' - -- | The response status code.
describeAggregateIdFormatResponse
    :: Int -- ^ 'daifrsResponseStatus'
    -> DescribeAggregateIdFormatResponse
describeAggregateIdFormatResponse pResponseStatus_ =
  DescribeAggregateIdFormatResponse'
    { _daifrsUseLongIdsAggregated = Nothing
    , _daifrsStatuses = Nothing
    , _daifrsResponseStatus = pResponseStatus_
    }


-- | Indicates whether all resource types in the region are configured to use longer IDs. This value is only @true@ if all users are configured to use longer IDs for all resources types in the region.
daifrsUseLongIdsAggregated :: Lens' DescribeAggregateIdFormatResponse (Maybe Bool)
daifrsUseLongIdsAggregated = lens _daifrsUseLongIdsAggregated (\ s a -> s{_daifrsUseLongIdsAggregated = a})

-- | Information about each resource's ID format.
daifrsStatuses :: Lens' DescribeAggregateIdFormatResponse [IdFormat]
daifrsStatuses = lens _daifrsStatuses (\ s a -> s{_daifrsStatuses = a}) . _Default . _Coerce

-- | -- | The response status code.
daifrsResponseStatus :: Lens' DescribeAggregateIdFormatResponse Int
daifrsResponseStatus = lens _daifrsResponseStatus (\ s a -> s{_daifrsResponseStatus = a})

instance NFData DescribeAggregateIdFormatResponse
         where
