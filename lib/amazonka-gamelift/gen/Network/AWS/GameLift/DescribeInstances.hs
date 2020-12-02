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
-- Module      : Network.AWS.GameLift.DescribeInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a fleet's instances, including instance IDs. Use this action to get details on all instances in the fleet or get details on one specific instance.
--
--
-- To get a specific instance, specify fleet ID and instance ID. To get all instances in a fleet, specify a fleet ID only. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, an 'Instance' object is returned for each result.
--
module Network.AWS.GameLift.DescribeInstances
    (
    -- * Creating a Request
      describeInstances
    , DescribeInstances
    -- * Request Lenses
    , diInstanceId
    , diNextToken
    , diLimit
    , diFleetId

    -- * Destructuring the Response
    , describeInstancesResponse
    , DescribeInstancesResponse
    -- * Response Lenses
    , dirsNextToken
    , dirsInstances
    , dirsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'describeInstances' smart constructor.
data DescribeInstances = DescribeInstances'
  { _diInstanceId :: !(Maybe Text)
  , _diNextToken  :: !(Maybe Text)
  , _diLimit      :: !(Maybe Nat)
  , _diFleetId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diInstanceId' - Unique identifier for an instance to retrieve. Specify an instance ID or leave blank to retrieve all instances in the fleet.
--
-- * 'diNextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
--
-- * 'diLimit' - Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- * 'diFleetId' - Unique identifier for a fleet to retrieve instance information for.
describeInstances
    :: Text -- ^ 'diFleetId'
    -> DescribeInstances
describeInstances pFleetId_ =
  DescribeInstances'
    { _diInstanceId = Nothing
    , _diNextToken = Nothing
    , _diLimit = Nothing
    , _diFleetId = pFleetId_
    }


-- | Unique identifier for an instance to retrieve. Specify an instance ID or leave blank to retrieve all instances in the fleet.
diInstanceId :: Lens' DescribeInstances (Maybe Text)
diInstanceId = lens _diInstanceId (\ s a -> s{_diInstanceId = a})

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
diNextToken :: Lens' DescribeInstances (Maybe Text)
diNextToken = lens _diNextToken (\ s a -> s{_diNextToken = a})

-- | Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
diLimit :: Lens' DescribeInstances (Maybe Natural)
diLimit = lens _diLimit (\ s a -> s{_diLimit = a}) . mapping _Nat

-- | Unique identifier for a fleet to retrieve instance information for.
diFleetId :: Lens' DescribeInstances Text
diFleetId = lens _diFleetId (\ s a -> s{_diFleetId = a})

instance AWSRequest DescribeInstances where
        type Rs DescribeInstances = DescribeInstancesResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribeInstancesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Instances" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeInstances where

instance NFData DescribeInstances where

instance ToHeaders DescribeInstances where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribeInstances" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeInstances where
        toJSON DescribeInstances'{..}
          = object
              (catMaybes
                 [("InstanceId" .=) <$> _diInstanceId,
                  ("NextToken" .=) <$> _diNextToken,
                  ("Limit" .=) <$> _diLimit,
                  Just ("FleetId" .= _diFleetId)])

instance ToPath DescribeInstances where
        toPath = const "/"

instance ToQuery DescribeInstances where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'describeInstancesResponse' smart constructor.
data DescribeInstancesResponse = DescribeInstancesResponse'
  { _dirsNextToken      :: !(Maybe Text)
  , _dirsInstances      :: !(Maybe [Instance])
  , _dirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsNextToken' - Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
--
-- * 'dirsInstances' - Collection of objects containing properties for each instance returned.
--
-- * 'dirsResponseStatus' - -- | The response status code.
describeInstancesResponse
    :: Int -- ^ 'dirsResponseStatus'
    -> DescribeInstancesResponse
describeInstancesResponse pResponseStatus_ =
  DescribeInstancesResponse'
    { _dirsNextToken = Nothing
    , _dirsInstances = Nothing
    , _dirsResponseStatus = pResponseStatus_
    }


-- | Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
dirsNextToken :: Lens' DescribeInstancesResponse (Maybe Text)
dirsNextToken = lens _dirsNextToken (\ s a -> s{_dirsNextToken = a})

-- | Collection of objects containing properties for each instance returned.
dirsInstances :: Lens' DescribeInstancesResponse [Instance]
dirsInstances = lens _dirsInstances (\ s a -> s{_dirsInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
dirsResponseStatus :: Lens' DescribeInstancesResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\ s a -> s{_dirsResponseStatus = a})

instance NFData DescribeInstancesResponse where
