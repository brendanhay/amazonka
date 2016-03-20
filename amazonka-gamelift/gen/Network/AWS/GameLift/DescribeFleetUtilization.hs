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
-- Module      : Network.AWS.GameLift.DescribeFleetUtilization
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves utilization statistics for one or more fleets. You can request
-- utilization data for all fleets, or specify a list of one or more fleet
-- IDs. When requesting all fleets, use the pagination parameters to
-- retrieve results as a set of sequential pages. If successful, a
-- < FleetUtilization> object is returned for each requested fleet ID. When
-- specifying a list of fleet IDs, utilization objects are returned only
-- for fleets that currently exist.
--
-- Some API actions may limit the number of fleet IDs allowed in one
-- request. If a request exceeds this limit, the request fails and the
-- error message includes the maximum allowed.
module Network.AWS.GameLift.DescribeFleetUtilization
    (
    -- * Creating a Request
      describeFleetUtilization
    , DescribeFleetUtilization
    -- * Request Lenses
    , dfuNextToken
    , dfuLimit
    , dfuFleetIds

    -- * Destructuring the Response
    , describeFleetUtilizationResponse
    , DescribeFleetUtilizationResponse
    -- * Response Lenses
    , dfursNextToken
    , dfursFleetUtilization
    , dfursResponseStatus
    ) where

import           Network.AWS.GameLift.Types
import           Network.AWS.GameLift.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for a request action.
--
-- /See:/ 'describeFleetUtilization' smart constructor.
data DescribeFleetUtilization = DescribeFleetUtilization'
    { _dfuNextToken :: !(Maybe Text)
    , _dfuLimit     :: !(Maybe Nat)
    , _dfuFleetIds  :: !(Maybe (List1 Text))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeFleetUtilization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfuNextToken'
--
-- * 'dfuLimit'
--
-- * 'dfuFleetIds'
describeFleetUtilization
    :: DescribeFleetUtilization
describeFleetUtilization =
    DescribeFleetUtilization'
    { _dfuNextToken = Nothing
    , _dfuLimit = Nothing
    , _dfuFleetIds = Nothing
    }

-- | Token indicating the start of the next sequential page of results. Use
-- the token that is returned with a previous call to this action. To
-- specify the start of the result set, do not specify a value. This
-- parameter is ignored when the request specifies one or a list of fleet
-- IDs.
dfuNextToken :: Lens' DescribeFleetUtilization (Maybe Text)
dfuNextToken = lens _dfuNextToken (\ s a -> s{_dfuNextToken = a});

-- | Maximum number of results to return. You can use this parameter with
-- /NextToken/ to get results as a set of sequential pages. This parameter
-- is ignored when the request specifies one or a list of fleet IDs.
dfuLimit :: Lens' DescribeFleetUtilization (Maybe Natural)
dfuLimit = lens _dfuLimit (\ s a -> s{_dfuLimit = a}) . mapping _Nat;

-- | Unique identifier for the fleet(s) you want to retrieve utilization data
-- for. Leave this parameter empty to retrieve utilization data for all
-- fleets.
dfuFleetIds :: Lens' DescribeFleetUtilization (Maybe (NonEmpty Text))
dfuFleetIds = lens _dfuFleetIds (\ s a -> s{_dfuFleetIds = a}) . mapping _List1;

instance AWSRequest DescribeFleetUtilization where
        type Rs DescribeFleetUtilization =
             DescribeFleetUtilizationResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribeFleetUtilizationResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "FleetUtilization" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeFleetUtilization

instance ToHeaders DescribeFleetUtilization where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribeFleetUtilization" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeFleetUtilization where
        toJSON DescribeFleetUtilization'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dfuNextToken,
                  ("Limit" .=) <$> _dfuLimit,
                  ("FleetIds" .=) <$> _dfuFleetIds])

instance ToPath DescribeFleetUtilization where
        toPath = const "/"

instance ToQuery DescribeFleetUtilization where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
-- /See:/ 'describeFleetUtilizationResponse' smart constructor.
data DescribeFleetUtilizationResponse = DescribeFleetUtilizationResponse'
    { _dfursNextToken        :: !(Maybe Text)
    , _dfursFleetUtilization :: !(Maybe [FleetUtilization])
    , _dfursResponseStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeFleetUtilizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfursNextToken'
--
-- * 'dfursFleetUtilization'
--
-- * 'dfursResponseStatus'
describeFleetUtilizationResponse
    :: Int -- ^ 'dfursResponseStatus'
    -> DescribeFleetUtilizationResponse
describeFleetUtilizationResponse pResponseStatus_ =
    DescribeFleetUtilizationResponse'
    { _dfursNextToken = Nothing
    , _dfursFleetUtilization = Nothing
    , _dfursResponseStatus = pResponseStatus_
    }

-- | Token indicating where to resume retrieving results on the next call to
-- this action. If no token is returned, these results represent the end of
-- the list.
--
-- If a request has a limit that exactly matches the number of remaining
-- results, a token is returned even though there are no more results to
-- retrieve.
dfursNextToken :: Lens' DescribeFleetUtilizationResponse (Maybe Text)
dfursNextToken = lens _dfursNextToken (\ s a -> s{_dfursNextToken = a});

-- | Collection of objects containing utilization information for each
-- requested fleet ID.
dfursFleetUtilization :: Lens' DescribeFleetUtilizationResponse [FleetUtilization]
dfursFleetUtilization = lens _dfursFleetUtilization (\ s a -> s{_dfursFleetUtilization = a}) . _Default . _Coerce;

-- | The response status code.
dfursResponseStatus :: Lens' DescribeFleetUtilizationResponse Int
dfursResponseStatus = lens _dfursResponseStatus (\ s a -> s{_dfursResponseStatus = a});
