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
-- Module      : Network.AWS.CloudWatchLogs.DescribeQueryDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns a paginated list of your saved CloudWatch Logs Insights query definitions.
--
--
-- You can use the @queryDefinitionNamePrefix@ parameter to limit the results to only the query definitions that have names that start with a certain string.
module Network.AWS.CloudWatchLogs.DescribeQueryDefinitions
  ( -- * Creating a Request
    describeQueryDefinitions,
    DescribeQueryDefinitions,

    -- * Request Lenses
    dqdQueryDefinitionNamePrefix,
    dqdNextToken,
    dqdMaxResults,

    -- * Destructuring the Response
    describeQueryDefinitionsResponse,
    DescribeQueryDefinitionsResponse,

    -- * Response Lenses
    dqdrsQueryDefinitions,
    dqdrsNextToken,
    dqdrsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeQueryDefinitions' smart constructor.
data DescribeQueryDefinitions = DescribeQueryDefinitions'
  { _dqdQueryDefinitionNamePrefix ::
      !(Maybe Text),
    _dqdNextToken :: !(Maybe Text),
    _dqdMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeQueryDefinitions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dqdQueryDefinitionNamePrefix' - Use this parameter to filter your results to only the query definitions that have names that start with the prefix you specify.
--
-- * 'dqdNextToken' - Undocumented member.
--
-- * 'dqdMaxResults' - Limits the number of returned query definitions to the specified number.
describeQueryDefinitions ::
  DescribeQueryDefinitions
describeQueryDefinitions =
  DescribeQueryDefinitions'
    { _dqdQueryDefinitionNamePrefix =
        Nothing,
      _dqdNextToken = Nothing,
      _dqdMaxResults = Nothing
    }

-- | Use this parameter to filter your results to only the query definitions that have names that start with the prefix you specify.
dqdQueryDefinitionNamePrefix :: Lens' DescribeQueryDefinitions (Maybe Text)
dqdQueryDefinitionNamePrefix = lens _dqdQueryDefinitionNamePrefix (\s a -> s {_dqdQueryDefinitionNamePrefix = a})

-- | Undocumented member.
dqdNextToken :: Lens' DescribeQueryDefinitions (Maybe Text)
dqdNextToken = lens _dqdNextToken (\s a -> s {_dqdNextToken = a})

-- | Limits the number of returned query definitions to the specified number.
dqdMaxResults :: Lens' DescribeQueryDefinitions (Maybe Natural)
dqdMaxResults = lens _dqdMaxResults (\s a -> s {_dqdMaxResults = a}) . mapping _Nat

instance AWSRequest DescribeQueryDefinitions where
  type Rs DescribeQueryDefinitions = DescribeQueryDefinitionsResponse
  request = postJSON cloudWatchLogs
  response =
    receiveJSON
      ( \s h x ->
          DescribeQueryDefinitionsResponse'
            <$> (x .?> "queryDefinitions" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeQueryDefinitions

instance NFData DescribeQueryDefinitions

instance ToHeaders DescribeQueryDefinitions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Logs_20140328.DescribeQueryDefinitions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeQueryDefinitions where
  toJSON DescribeQueryDefinitions' {..} =
    object
      ( catMaybes
          [ ("queryDefinitionNamePrefix" .=)
              <$> _dqdQueryDefinitionNamePrefix,
            ("nextToken" .=) <$> _dqdNextToken,
            ("maxResults" .=) <$> _dqdMaxResults
          ]
      )

instance ToPath DescribeQueryDefinitions where
  toPath = const "/"

instance ToQuery DescribeQueryDefinitions where
  toQuery = const mempty

-- | /See:/ 'describeQueryDefinitionsResponse' smart constructor.
data DescribeQueryDefinitionsResponse = DescribeQueryDefinitionsResponse'
  { _dqdrsQueryDefinitions ::
      !( Maybe
           [QueryDefinition]
       ),
    _dqdrsNextToken ::
      !(Maybe Text),
    _dqdrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeQueryDefinitionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dqdrsQueryDefinitions' - The list of query definitions that match your request.
--
-- * 'dqdrsNextToken' - Undocumented member.
--
-- * 'dqdrsResponseStatus' - -- | The response status code.
describeQueryDefinitionsResponse ::
  -- | 'dqdrsResponseStatus'
  Int ->
  DescribeQueryDefinitionsResponse
describeQueryDefinitionsResponse pResponseStatus_ =
  DescribeQueryDefinitionsResponse'
    { _dqdrsQueryDefinitions =
        Nothing,
      _dqdrsNextToken = Nothing,
      _dqdrsResponseStatus = pResponseStatus_
    }

-- | The list of query definitions that match your request.
dqdrsQueryDefinitions :: Lens' DescribeQueryDefinitionsResponse [QueryDefinition]
dqdrsQueryDefinitions = lens _dqdrsQueryDefinitions (\s a -> s {_dqdrsQueryDefinitions = a}) . _Default . _Coerce

-- | Undocumented member.
dqdrsNextToken :: Lens' DescribeQueryDefinitionsResponse (Maybe Text)
dqdrsNextToken = lens _dqdrsNextToken (\s a -> s {_dqdrsNextToken = a})

-- | -- | The response status code.
dqdrsResponseStatus :: Lens' DescribeQueryDefinitionsResponse Int
dqdrsResponseStatus = lens _dqdrsResponseStatus (\s a -> s {_dqdrsResponseStatus = a})

instance NFData DescribeQueryDefinitionsResponse
