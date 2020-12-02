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
-- Module      : Network.AWS.SSM.DescribeAssociationExecutionTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API action to view information about a specific execution of a specific association.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeAssociationExecutionTargets
  ( -- * Creating a Request
    describeAssociationExecutionTargets,
    DescribeAssociationExecutionTargets,

    -- * Request Lenses
    daetFilters,
    daetNextToken,
    daetMaxResults,
    daetAssociationId,
    daetExecutionId,

    -- * Destructuring the Response
    describeAssociationExecutionTargetsResponse,
    DescribeAssociationExecutionTargetsResponse,

    -- * Response Lenses
    daetrsNextToken,
    daetrsAssociationExecutionTargets,
    daetrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'describeAssociationExecutionTargets' smart constructor.
data DescribeAssociationExecutionTargets = DescribeAssociationExecutionTargets'
  { _daetFilters ::
      !( Maybe
           ( List1
               AssociationExecutionTargetsFilter
           )
       ),
    _daetNextToken ::
      !(Maybe Text),
    _daetMaxResults ::
      !(Maybe Nat),
    _daetAssociationId ::
      !Text,
    _daetExecutionId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAssociationExecutionTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daetFilters' - Filters for the request. You can specify the following filters and values. Status (EQUAL) ResourceId (EQUAL) ResourceType (EQUAL)
--
-- * 'daetNextToken' - A token to start the list. Use this token to get the next set of results.
--
-- * 'daetMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'daetAssociationId' - The association ID that includes the execution for which you want to view details.
--
-- * 'daetExecutionId' - The execution ID for which you want to view details.
describeAssociationExecutionTargets ::
  -- | 'daetAssociationId'
  Text ->
  -- | 'daetExecutionId'
  Text ->
  DescribeAssociationExecutionTargets
describeAssociationExecutionTargets pAssociationId_ pExecutionId_ =
  DescribeAssociationExecutionTargets'
    { _daetFilters = Nothing,
      _daetNextToken = Nothing,
      _daetMaxResults = Nothing,
      _daetAssociationId = pAssociationId_,
      _daetExecutionId = pExecutionId_
    }

-- | Filters for the request. You can specify the following filters and values. Status (EQUAL) ResourceId (EQUAL) ResourceType (EQUAL)
daetFilters :: Lens' DescribeAssociationExecutionTargets (Maybe (NonEmpty AssociationExecutionTargetsFilter))
daetFilters = lens _daetFilters (\s a -> s {_daetFilters = a}) . mapping _List1

-- | A token to start the list. Use this token to get the next set of results.
daetNextToken :: Lens' DescribeAssociationExecutionTargets (Maybe Text)
daetNextToken = lens _daetNextToken (\s a -> s {_daetNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
daetMaxResults :: Lens' DescribeAssociationExecutionTargets (Maybe Natural)
daetMaxResults = lens _daetMaxResults (\s a -> s {_daetMaxResults = a}) . mapping _Nat

-- | The association ID that includes the execution for which you want to view details.
daetAssociationId :: Lens' DescribeAssociationExecutionTargets Text
daetAssociationId = lens _daetAssociationId (\s a -> s {_daetAssociationId = a})

-- | The execution ID for which you want to view details.
daetExecutionId :: Lens' DescribeAssociationExecutionTargets Text
daetExecutionId = lens _daetExecutionId (\s a -> s {_daetExecutionId = a})

instance AWSPager DescribeAssociationExecutionTargets where
  page rq rs
    | stop (rs ^. daetrsNextToken) = Nothing
    | stop (rs ^. daetrsAssociationExecutionTargets) = Nothing
    | otherwise = Just $ rq & daetNextToken .~ rs ^. daetrsNextToken

instance AWSRequest DescribeAssociationExecutionTargets where
  type
    Rs DescribeAssociationExecutionTargets =
      DescribeAssociationExecutionTargetsResponse
  request = postJSON ssm
  response =
    receiveJSON
      ( \s h x ->
          DescribeAssociationExecutionTargetsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "AssociationExecutionTargets" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeAssociationExecutionTargets

instance NFData DescribeAssociationExecutionTargets

instance ToHeaders DescribeAssociationExecutionTargets where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonSSM.DescribeAssociationExecutionTargets" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeAssociationExecutionTargets where
  toJSON DescribeAssociationExecutionTargets' {..} =
    object
      ( catMaybes
          [ ("Filters" .=) <$> _daetFilters,
            ("NextToken" .=) <$> _daetNextToken,
            ("MaxResults" .=) <$> _daetMaxResults,
            Just ("AssociationId" .= _daetAssociationId),
            Just ("ExecutionId" .= _daetExecutionId)
          ]
      )

instance ToPath DescribeAssociationExecutionTargets where
  toPath = const "/"

instance ToQuery DescribeAssociationExecutionTargets where
  toQuery = const mempty

-- | /See:/ 'describeAssociationExecutionTargetsResponse' smart constructor.
data DescribeAssociationExecutionTargetsResponse = DescribeAssociationExecutionTargetsResponse'
  { _daetrsNextToken ::
      !( Maybe
           Text
       ),
    _daetrsAssociationExecutionTargets ::
      !( Maybe
           [AssociationExecutionTarget]
       ),
    _daetrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeAssociationExecutionTargetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daetrsNextToken' - The token for the next set of items to return. Use this token to get the next set of results.
--
-- * 'daetrsAssociationExecutionTargets' - Information about the execution.
--
-- * 'daetrsResponseStatus' - -- | The response status code.
describeAssociationExecutionTargetsResponse ::
  -- | 'daetrsResponseStatus'
  Int ->
  DescribeAssociationExecutionTargetsResponse
describeAssociationExecutionTargetsResponse pResponseStatus_ =
  DescribeAssociationExecutionTargetsResponse'
    { _daetrsNextToken =
        Nothing,
      _daetrsAssociationExecutionTargets = Nothing,
      _daetrsResponseStatus = pResponseStatus_
    }

-- | The token for the next set of items to return. Use this token to get the next set of results.
daetrsNextToken :: Lens' DescribeAssociationExecutionTargetsResponse (Maybe Text)
daetrsNextToken = lens _daetrsNextToken (\s a -> s {_daetrsNextToken = a})

-- | Information about the execution.
daetrsAssociationExecutionTargets :: Lens' DescribeAssociationExecutionTargetsResponse [AssociationExecutionTarget]
daetrsAssociationExecutionTargets = lens _daetrsAssociationExecutionTargets (\s a -> s {_daetrsAssociationExecutionTargets = a}) . _Default . _Coerce

-- | -- | The response status code.
daetrsResponseStatus :: Lens' DescribeAssociationExecutionTargetsResponse Int
daetrsResponseStatus = lens _daetrsResponseStatus (\s a -> s {_daetrsResponseStatus = a})

instance NFData DescribeAssociationExecutionTargetsResponse
