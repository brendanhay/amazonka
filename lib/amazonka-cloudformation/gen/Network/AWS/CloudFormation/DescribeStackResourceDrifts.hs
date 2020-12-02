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
-- Module      : Network.AWS.CloudFormation.DescribeStackResourceDrifts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns drift information for the resources that have been checked for drift in the specified stack. This includes actual and expected configuration values for resources where AWS CloudFormation detects configuration drift.
--
--
-- For a given stack, there will be one @StackResourceDrift@ for each stack resource that has been checked for drift. Resources that have not yet been checked for drift are not included. Resources that do not currently support drift detection are not checked, and so not included. For a list of resources that support drift detection, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
--
-- Use 'DetectStackResourceDrift' to detect drift on individual resources, or 'DetectStackDrift' to detect drift on all supported resources for a given stack.
module Network.AWS.CloudFormation.DescribeStackResourceDrifts
  ( -- * Creating a Request
    describeStackResourceDrifts,
    DescribeStackResourceDrifts,

    -- * Request Lenses
    dsrdNextToken,
    dsrdMaxResults,
    dsrdStackResourceDriftStatusFilters,
    dsrdStackName,

    -- * Destructuring the Response
    describeStackResourceDriftsResponse,
    DescribeStackResourceDriftsResponse,

    -- * Response Lenses
    drsNextToken,
    drsResponseStatus,
    drsStackResourceDrifts,
  )
where

import Network.AWS.CloudFormation.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeStackResourceDrifts' smart constructor.
data DescribeStackResourceDrifts = DescribeStackResourceDrifts'
  { _dsrdNextToken ::
      !(Maybe Text),
    _dsrdMaxResults :: !(Maybe Nat),
    _dsrdStackResourceDriftStatusFilters ::
      !( Maybe
           ( List1
               StackResourceDriftStatus
           )
       ),
    _dsrdStackName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeStackResourceDrifts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrdNextToken' - A string that identifies the next page of stack resource drift results.
--
-- * 'dsrdMaxResults' - The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- * 'dsrdStackResourceDriftStatusFilters' - The resource drift status values to use as filters for the resource drift results returned.     * @DELETED@ : The resource differs from its expected template configuration in that the resource has been deleted.     * @MODIFIED@ : One or more resource properties differ from their expected template values.     * @IN_SYNC@ : The resources's actual configuration matches its expected template configuration.     * @NOT_CHECKED@ : AWS CloudFormation does not currently return this value.
--
-- * 'dsrdStackName' - The name of the stack for which you want drift information.
describeStackResourceDrifts ::
  -- | 'dsrdStackName'
  Text ->
  DescribeStackResourceDrifts
describeStackResourceDrifts pStackName_ =
  DescribeStackResourceDrifts'
    { _dsrdNextToken = Nothing,
      _dsrdMaxResults = Nothing,
      _dsrdStackResourceDriftStatusFilters = Nothing,
      _dsrdStackName = pStackName_
    }

-- | A string that identifies the next page of stack resource drift results.
dsrdNextToken :: Lens' DescribeStackResourceDrifts (Maybe Text)
dsrdNextToken = lens _dsrdNextToken (\s a -> s {_dsrdNextToken = a})

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
dsrdMaxResults :: Lens' DescribeStackResourceDrifts (Maybe Natural)
dsrdMaxResults = lens _dsrdMaxResults (\s a -> s {_dsrdMaxResults = a}) . mapping _Nat

-- | The resource drift status values to use as filters for the resource drift results returned.     * @DELETED@ : The resource differs from its expected template configuration in that the resource has been deleted.     * @MODIFIED@ : One or more resource properties differ from their expected template values.     * @IN_SYNC@ : The resources's actual configuration matches its expected template configuration.     * @NOT_CHECKED@ : AWS CloudFormation does not currently return this value.
dsrdStackResourceDriftStatusFilters :: Lens' DescribeStackResourceDrifts (Maybe (NonEmpty StackResourceDriftStatus))
dsrdStackResourceDriftStatusFilters = lens _dsrdStackResourceDriftStatusFilters (\s a -> s {_dsrdStackResourceDriftStatusFilters = a}) . mapping _List1

-- | The name of the stack for which you want drift information.
dsrdStackName :: Lens' DescribeStackResourceDrifts Text
dsrdStackName = lens _dsrdStackName (\s a -> s {_dsrdStackName = a})

instance AWSRequest DescribeStackResourceDrifts where
  type
    Rs DescribeStackResourceDrifts =
      DescribeStackResourceDriftsResponse
  request = postQuery cloudFormation
  response =
    receiveXMLWrapper
      "DescribeStackResourceDriftsResult"
      ( \s h x ->
          DescribeStackResourceDriftsResponse'
            <$> (x .@? "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .@? "StackResourceDrifts" .!@ mempty >>= parseXMLList "member")
      )

instance Hashable DescribeStackResourceDrifts

instance NFData DescribeStackResourceDrifts

instance ToHeaders DescribeStackResourceDrifts where
  toHeaders = const mempty

instance ToPath DescribeStackResourceDrifts where
  toPath = const "/"

instance ToQuery DescribeStackResourceDrifts where
  toQuery DescribeStackResourceDrifts' {..} =
    mconcat
      [ "Action" =: ("DescribeStackResourceDrifts" :: ByteString),
        "Version" =: ("2010-05-15" :: ByteString),
        "NextToken" =: _dsrdNextToken,
        "MaxResults" =: _dsrdMaxResults,
        "StackResourceDriftStatusFilters"
          =: toQuery
            (toQueryList "member" <$> _dsrdStackResourceDriftStatusFilters),
        "StackName" =: _dsrdStackName
      ]

-- | /See:/ 'describeStackResourceDriftsResponse' smart constructor.
data DescribeStackResourceDriftsResponse = DescribeStackResourceDriftsResponse'
  { _drsNextToken ::
      !(Maybe Text),
    _drsResponseStatus ::
      !Int,
    _drsStackResourceDrifts ::
      ![StackResourceDrift]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeStackResourceDriftsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsNextToken' - If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @DescribeStackResourceDrifts@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
--
-- * 'drsResponseStatus' - -- | The response status code.
--
-- * 'drsStackResourceDrifts' - Drift information for the resources that have been checked for drift in the specified stack. This includes actual and expected configuration values for resources where AWS CloudFormation detects drift. For a given stack, there will be one @StackResourceDrift@ for each stack resource that has been checked for drift. Resources that have not yet been checked for drift are not included. Resources that do not currently support drift detection are not checked, and so not included. For a list of resources that support drift detection, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
describeStackResourceDriftsResponse ::
  -- | 'drsResponseStatus'
  Int ->
  DescribeStackResourceDriftsResponse
describeStackResourceDriftsResponse pResponseStatus_ =
  DescribeStackResourceDriftsResponse'
    { _drsNextToken = Nothing,
      _drsResponseStatus = pResponseStatus_,
      _drsStackResourceDrifts = mempty
    }

-- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @DescribeStackResourceDrifts@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
drsNextToken :: Lens' DescribeStackResourceDriftsResponse (Maybe Text)
drsNextToken = lens _drsNextToken (\s a -> s {_drsNextToken = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeStackResourceDriftsResponse Int
drsResponseStatus = lens _drsResponseStatus (\s a -> s {_drsResponseStatus = a})

-- | Drift information for the resources that have been checked for drift in the specified stack. This includes actual and expected configuration values for resources where AWS CloudFormation detects drift. For a given stack, there will be one @StackResourceDrift@ for each stack resource that has been checked for drift. Resources that have not yet been checked for drift are not included. Resources that do not currently support drift detection are not checked, and so not included. For a list of resources that support drift detection, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
drsStackResourceDrifts :: Lens' DescribeStackResourceDriftsResponse [StackResourceDrift]
drsStackResourceDrifts = lens _drsStackResourceDrifts (\s a -> s {_drsStackResourceDrifts = a}) . _Coerce

instance NFData DescribeStackResourceDriftsResponse
