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
-- Module      : Network.AWS.GameLift.ListFleets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a collection of fleet resources for this AWS account. You can filter the result set to find only those fleets that are deployed with a specific build or script. Use the pagination parameters to retrieve results in sequential pages.
--
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
--     * 'DescribeFleetAttributes'
--
--     * 'UpdateFleetAttributes'
--
--     * 'StartFleetActions' or 'StopFleetActions'
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListFleets
  ( -- * Creating a Request
    listFleets,
    ListFleets,

    -- * Request Lenses
    lfBuildId,
    lfNextToken,
    lfScriptId,
    lfLimit,

    -- * Destructuring the Response
    listFleetsResponse,
    ListFleetsResponse,

    -- * Response Lenses
    lfrsNextToken,
    lfrsFleetIds,
    lfrsResponseStatus,
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
-- /See:/ 'listFleets' smart constructor.
data ListFleets = ListFleets'
  { _lfBuildId :: !(Maybe Text),
    _lfNextToken :: !(Maybe Text),
    _lfScriptId :: !(Maybe Text),
    _lfLimit :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListFleets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfBuildId' - A unique identifier for a build to return fleets for. Use this parameter to return only fleets using a specified build. Use either the build ID or ARN value. To retrieve all fleets, do not include either a BuildId and ScriptID parameter.
--
-- * 'lfNextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- * 'lfScriptId' - A unique identifier for a Realtime script to return fleets for. Use this parameter to return only fleets using a specified script. Use either the script ID or ARN value. To retrieve all fleets, leave this parameter empty.
--
-- * 'lfLimit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
listFleets ::
  ListFleets
listFleets =
  ListFleets'
    { _lfBuildId = Nothing,
      _lfNextToken = Nothing,
      _lfScriptId = Nothing,
      _lfLimit = Nothing
    }

-- | A unique identifier for a build to return fleets for. Use this parameter to return only fleets using a specified build. Use either the build ID or ARN value. To retrieve all fleets, do not include either a BuildId and ScriptID parameter.
lfBuildId :: Lens' ListFleets (Maybe Text)
lfBuildId = lens _lfBuildId (\s a -> s {_lfBuildId = a})

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
lfNextToken :: Lens' ListFleets (Maybe Text)
lfNextToken = lens _lfNextToken (\s a -> s {_lfNextToken = a})

-- | A unique identifier for a Realtime script to return fleets for. Use this parameter to return only fleets using a specified script. Use either the script ID or ARN value. To retrieve all fleets, leave this parameter empty.
lfScriptId :: Lens' ListFleets (Maybe Text)
lfScriptId = lens _lfScriptId (\s a -> s {_lfScriptId = a})

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
lfLimit :: Lens' ListFleets (Maybe Natural)
lfLimit = lens _lfLimit (\s a -> s {_lfLimit = a}) . mapping _Nat

instance AWSPager ListFleets where
  page rq rs
    | stop (rs ^. lfrsNextToken) = Nothing
    | stop (rs ^. lfrsFleetIds) = Nothing
    | otherwise = Just $ rq & lfNextToken .~ rs ^. lfrsNextToken

instance AWSRequest ListFleets where
  type Rs ListFleets = ListFleetsResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          ListFleetsResponse'
            <$> (x .?> "NextToken") <*> (x .?> "FleetIds") <*> (pure (fromEnum s))
      )

instance Hashable ListFleets

instance NFData ListFleets

instance ToHeaders ListFleets where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.ListFleets" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListFleets where
  toJSON ListFleets' {..} =
    object
      ( catMaybes
          [ ("BuildId" .=) <$> _lfBuildId,
            ("NextToken" .=) <$> _lfNextToken,
            ("ScriptId" .=) <$> _lfScriptId,
            ("Limit" .=) <$> _lfLimit
          ]
      )

instance ToPath ListFleets where
  toPath = const "/"

instance ToQuery ListFleets where
  toQuery = const mempty

-- | Represents the returned data in response to a request operation.
--
--
--
-- /See:/ 'listFleetsResponse' smart constructor.
data ListFleetsResponse = ListFleetsResponse'
  { _lfrsNextToken ::
      !(Maybe Text),
    _lfrsFleetIds :: !(Maybe (List1 Text)),
    _lfrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListFleetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfrsNextToken' - Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- * 'lfrsFleetIds' - Set of fleet IDs matching the list request. You can retrieve additional information about all returned fleets by passing this result set to a call to 'DescribeFleetAttributes' , 'DescribeFleetCapacity' , or 'DescribeFleetUtilization' .
--
-- * 'lfrsResponseStatus' - -- | The response status code.
listFleetsResponse ::
  -- | 'lfrsResponseStatus'
  Int ->
  ListFleetsResponse
listFleetsResponse pResponseStatus_ =
  ListFleetsResponse'
    { _lfrsNextToken = Nothing,
      _lfrsFleetIds = Nothing,
      _lfrsResponseStatus = pResponseStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
lfrsNextToken :: Lens' ListFleetsResponse (Maybe Text)
lfrsNextToken = lens _lfrsNextToken (\s a -> s {_lfrsNextToken = a})

-- | Set of fleet IDs matching the list request. You can retrieve additional information about all returned fleets by passing this result set to a call to 'DescribeFleetAttributes' , 'DescribeFleetCapacity' , or 'DescribeFleetUtilization' .
lfrsFleetIds :: Lens' ListFleetsResponse (Maybe (NonEmpty Text))
lfrsFleetIds = lens _lfrsFleetIds (\s a -> s {_lfrsFleetIds = a}) . mapping _List1

-- | -- | The response status code.
lfrsResponseStatus :: Lens' ListFleetsResponse Int
lfrsResponseStatus = lens _lfrsResponseStatus (\s a -> s {_lfrsResponseStatus = a})

instance NFData ListFleetsResponse
