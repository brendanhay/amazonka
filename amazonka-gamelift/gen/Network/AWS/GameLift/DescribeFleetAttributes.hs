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
-- Module      : Network.AWS.GameLift.DescribeFleetAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves fleet properties, including metadata, status, and configuration, for one or more fleets. You can request attributes for all fleets, or specify a list of one or more fleet IDs. When requesting multiple fleets, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'FleetAttributes' object is returned for each requested fleet ID. When specifying a list of fleet IDs, attribute objects are returned only for fleets that currently exist.
--
--
-- Fleet-related operations include:
--
--     * 'CreateFleet'
--
--     * 'ListFleets'
--
--     * 'DeleteFleet'
--
--     * Describe fleets:
--
--     * 'DescribeFleetAttributes'
--
--     * 'DescribeFleetCapacity'
--
--     * 'DescribeFleetPortSettings'
--
--     * 'DescribeFleetUtilization'
--
--     * 'DescribeRuntimeConfiguration'
--
--     * 'DescribeEC2InstanceLimits'
--
--     * 'DescribeFleetEvents'
--
--
--
--     * Update fleets:
--
--     * 'UpdateFleetAttributes'
--
--     * 'UpdateFleetCapacity'
--
--     * 'UpdateFleetPortSettings'
--
--     * 'UpdateRuntimeConfiguration'
--
--
--
--     * Manage fleet actions:
--
--     * 'StartFleetActions'
--
--     * 'StopFleetActions'
--
--
--
--
--
module Network.AWS.GameLift.DescribeFleetAttributes
    (
    -- * Creating a Request
      describeFleetAttributes
    , DescribeFleetAttributes
    -- * Request Lenses
    , dfaNextToken
    , dfaLimit
    , dfaFleetIds

    -- * Destructuring the Response
    , describeFleetAttributesResponse
    , DescribeFleetAttributesResponse
    -- * Response Lenses
    , dfarsNextToken
    , dfarsFleetAttributes
    , dfarsResponseStatus
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
-- /See:/ 'describeFleetAttributes' smart constructor.
data DescribeFleetAttributes = DescribeFleetAttributes'
  { _dfaNextToken :: !(Maybe Text)
  , _dfaLimit     :: !(Maybe Nat)
  , _dfaFleetIds  :: !(Maybe (List1 Text))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFleetAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfaNextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
--
-- * 'dfaLimit' - Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
--
-- * 'dfaFleetIds' - Unique identifier for a fleet(s) to retrieve attributes for. To request attributes for all fleets, leave this parameter empty.
describeFleetAttributes
    :: DescribeFleetAttributes
describeFleetAttributes =
  DescribeFleetAttributes'
    {_dfaNextToken = Nothing, _dfaLimit = Nothing, _dfaFleetIds = Nothing}


-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
dfaNextToken :: Lens' DescribeFleetAttributes (Maybe Text)
dfaNextToken = lens _dfaNextToken (\ s a -> s{_dfaNextToken = a})

-- | Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
dfaLimit :: Lens' DescribeFleetAttributes (Maybe Natural)
dfaLimit = lens _dfaLimit (\ s a -> s{_dfaLimit = a}) . mapping _Nat

-- | Unique identifier for a fleet(s) to retrieve attributes for. To request attributes for all fleets, leave this parameter empty.
dfaFleetIds :: Lens' DescribeFleetAttributes (Maybe (NonEmpty Text))
dfaFleetIds = lens _dfaFleetIds (\ s a -> s{_dfaFleetIds = a}) . mapping _List1

instance AWSRequest DescribeFleetAttributes where
        type Rs DescribeFleetAttributes =
             DescribeFleetAttributesResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribeFleetAttributesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "FleetAttributes" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeFleetAttributes where

instance NFData DescribeFleetAttributes where

instance ToHeaders DescribeFleetAttributes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribeFleetAttributes" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeFleetAttributes where
        toJSON DescribeFleetAttributes'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dfaNextToken,
                  ("Limit" .=) <$> _dfaLimit,
                  ("FleetIds" .=) <$> _dfaFleetIds])

instance ToPath DescribeFleetAttributes where
        toPath = const "/"

instance ToQuery DescribeFleetAttributes where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'describeFleetAttributesResponse' smart constructor.
data DescribeFleetAttributesResponse = DescribeFleetAttributesResponse'
  { _dfarsNextToken       :: !(Maybe Text)
  , _dfarsFleetAttributes :: !(Maybe [FleetAttributes])
  , _dfarsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFleetAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfarsNextToken' - Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
--
-- * 'dfarsFleetAttributes' - Collection of objects containing attribute metadata for each requested fleet ID.
--
-- * 'dfarsResponseStatus' - -- | The response status code.
describeFleetAttributesResponse
    :: Int -- ^ 'dfarsResponseStatus'
    -> DescribeFleetAttributesResponse
describeFleetAttributesResponse pResponseStatus_ =
  DescribeFleetAttributesResponse'
    { _dfarsNextToken = Nothing
    , _dfarsFleetAttributes = Nothing
    , _dfarsResponseStatus = pResponseStatus_
    }


-- | Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
dfarsNextToken :: Lens' DescribeFleetAttributesResponse (Maybe Text)
dfarsNextToken = lens _dfarsNextToken (\ s a -> s{_dfarsNextToken = a})

-- | Collection of objects containing attribute metadata for each requested fleet ID.
dfarsFleetAttributes :: Lens' DescribeFleetAttributesResponse [FleetAttributes]
dfarsFleetAttributes = lens _dfarsFleetAttributes (\ s a -> s{_dfarsFleetAttributes = a}) . _Default . _Coerce

-- | -- | The response status code.
dfarsResponseStatus :: Lens' DescribeFleetAttributesResponse Int
dfarsResponseStatus = lens _dfarsResponseStatus (\ s a -> s{_dfarsResponseStatus = a})

instance NFData DescribeFleetAttributesResponse where
