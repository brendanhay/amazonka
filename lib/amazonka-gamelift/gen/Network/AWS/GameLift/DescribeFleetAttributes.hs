{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeFleetAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves core properties, including configuration, status, and metadata, for a fleet.
--
--
-- To get attributes for one or more fleets, provide a list of fleet IDs or fleet ARNs. To get attributes for all fleets, do not specify a fleet identifier. When requesting attributes for multiple fleets, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'FleetAttributes' object is returned for each fleet requested, unless the fleet identifier is not found.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
--
-- __Related operations__
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
--     * 'UpdateFleetAttributes'
--
--     * 'StartFleetActions' or 'StopFleetActions'
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeFleetAttributes
  ( -- * Creating a Request
    describeFleetAttributes,
    DescribeFleetAttributes,

    -- * Request Lenses
    dfaNextToken,
    dfaLimit,
    dfaFleetIds,

    -- * Destructuring the Response
    describeFleetAttributesResponse,
    DescribeFleetAttributesResponse,

    -- * Response Lenses
    dfarsNextToken,
    dfarsFleetAttributes,
    dfarsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request operation.
--
--
--
-- /See:/ 'describeFleetAttributes' smart constructor.
data DescribeFleetAttributes = DescribeFleetAttributes'
  { _dfaNextToken ::
      !(Maybe Text),
    _dfaLimit :: !(Maybe Nat),
    _dfaFleetIds :: !(Maybe (List1 Text))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFleetAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfaNextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
--
-- * 'dfaLimit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
--
-- * 'dfaFleetIds' - A list of unique fleet identifiers to retrieve attributes for. You can use either the fleet ID or ARN value. To retrieve attributes for all current fleets, do not include this parameter. If the list of fleet identifiers includes fleets that don't currently exist, the request succeeds but no attributes for that fleet are returned.
describeFleetAttributes ::
  DescribeFleetAttributes
describeFleetAttributes =
  DescribeFleetAttributes'
    { _dfaNextToken = Nothing,
      _dfaLimit = Nothing,
      _dfaFleetIds = Nothing
    }

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
dfaNextToken :: Lens' DescribeFleetAttributes (Maybe Text)
dfaNextToken = lens _dfaNextToken (\s a -> s {_dfaNextToken = a})

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
dfaLimit :: Lens' DescribeFleetAttributes (Maybe Natural)
dfaLimit = lens _dfaLimit (\s a -> s {_dfaLimit = a}) . mapping _Nat

-- | A list of unique fleet identifiers to retrieve attributes for. You can use either the fleet ID or ARN value. To retrieve attributes for all current fleets, do not include this parameter. If the list of fleet identifiers includes fleets that don't currently exist, the request succeeds but no attributes for that fleet are returned.
dfaFleetIds :: Lens' DescribeFleetAttributes (Maybe (NonEmpty Text))
dfaFleetIds = lens _dfaFleetIds (\s a -> s {_dfaFleetIds = a}) . mapping _List1

instance AWSPager DescribeFleetAttributes where
  page rq rs
    | stop (rs ^. dfarsNextToken) = Nothing
    | stop (rs ^. dfarsFleetAttributes) = Nothing
    | otherwise = Just $ rq & dfaNextToken .~ rs ^. dfarsNextToken

instance AWSRequest DescribeFleetAttributes where
  type Rs DescribeFleetAttributes = DescribeFleetAttributesResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          DescribeFleetAttributesResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "FleetAttributes" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeFleetAttributes

instance NFData DescribeFleetAttributes

instance ToHeaders DescribeFleetAttributes where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("GameLift.DescribeFleetAttributes" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeFleetAttributes where
  toJSON DescribeFleetAttributes' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _dfaNextToken,
            ("Limit" .=) <$> _dfaLimit,
            ("FleetIds" .=) <$> _dfaFleetIds
          ]
      )

instance ToPath DescribeFleetAttributes where
  toPath = const "/"

instance ToQuery DescribeFleetAttributes where
  toQuery = const mempty

-- | Represents the returned data in response to a request operation.
--
--
--
-- /See:/ 'describeFleetAttributesResponse' smart constructor.
data DescribeFleetAttributesResponse = DescribeFleetAttributesResponse'
  { _dfarsNextToken ::
      !(Maybe Text),
    _dfarsFleetAttributes ::
      !(Maybe [FleetAttributes]),
    _dfarsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFleetAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfarsNextToken' - Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- * 'dfarsFleetAttributes' - A collection of objects containing attribute metadata for each requested fleet ID. Attribute objects are returned only for fleets that currently exist.
--
-- * 'dfarsResponseStatus' - -- | The response status code.
describeFleetAttributesResponse ::
  -- | 'dfarsResponseStatus'
  Int ->
  DescribeFleetAttributesResponse
describeFleetAttributesResponse pResponseStatus_ =
  DescribeFleetAttributesResponse'
    { _dfarsNextToken = Nothing,
      _dfarsFleetAttributes = Nothing,
      _dfarsResponseStatus = pResponseStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
dfarsNextToken :: Lens' DescribeFleetAttributesResponse (Maybe Text)
dfarsNextToken = lens _dfarsNextToken (\s a -> s {_dfarsNextToken = a})

-- | A collection of objects containing attribute metadata for each requested fleet ID. Attribute objects are returned only for fleets that currently exist.
dfarsFleetAttributes :: Lens' DescribeFleetAttributesResponse [FleetAttributes]
dfarsFleetAttributes = lens _dfarsFleetAttributes (\s a -> s {_dfarsFleetAttributes = a}) . _Default . _Coerce

-- | -- | The response status code.
dfarsResponseStatus :: Lens' DescribeFleetAttributesResponse Int
dfarsResponseStatus = lens _dfarsResponseStatus (\s a -> s {_dfarsResponseStatus = a})

instance NFData DescribeFleetAttributesResponse
