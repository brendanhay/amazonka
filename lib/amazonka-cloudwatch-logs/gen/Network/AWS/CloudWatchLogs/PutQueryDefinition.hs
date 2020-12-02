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
-- Module      : Network.AWS.CloudWatchLogs.PutQueryDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a query definition for CloudWatch Logs Insights. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/AnalyzingLogData.html Analyzing Log Data with CloudWatch Logs Insights> .
--
--
-- To update a query definition, specify its @queryDefinitionId@ in your request. The values of @name@ , @queryString@ , and @logGroupNames@ are changed to the values that you specify in your update operation. No current values are retained from the current query definition. For example, if you update a current query definition that includes log groups, and you don't specify the @logGroupNames@ parameter in your update operation, the query definition changes to contain no log groups.
--
-- You must have the @logs:PutQueryDefinition@ permission to be able to perform this operation.
module Network.AWS.CloudWatchLogs.PutQueryDefinition
  ( -- * Creating a Request
    putQueryDefinition,
    PutQueryDefinition,

    -- * Request Lenses
    pqdLogGroupNames,
    pqdQueryDefinitionId,
    pqdName,
    pqdQueryString,

    -- * Destructuring the Response
    putQueryDefinitionResponse,
    PutQueryDefinitionResponse,

    -- * Response Lenses
    pqdrsQueryDefinitionId,
    pqdrsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putQueryDefinition' smart constructor.
data PutQueryDefinition = PutQueryDefinition'
  { _pqdLogGroupNames ::
      !(Maybe [Text]),
    _pqdQueryDefinitionId :: !(Maybe Text),
    _pqdName :: !Text,
    _pqdQueryString :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutQueryDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pqdLogGroupNames' - Use this parameter to include specific log groups as part of your query definition. If you are updating a query definition and you omit this parameter, then the updated definition will contain no log groups.
--
-- * 'pqdQueryDefinitionId' - If you are updating a query definition, use this parameter to specify the ID of the query definition that you want to update. You can use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions> to retrieve the IDs of your saved query definitions. If you are creating a query definition, do not specify this parameter. CloudWatch generates a unique ID for the new query definition and include it in the response to this operation.
--
-- * 'pqdName' - A name for the query definition. If you are saving a lot of query definitions, we recommend that you name them so that you can easily find the ones you want by using the first part of the name as a filter in the @queryDefinitionNamePrefix@ parameter of <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions> .
--
-- * 'pqdQueryString' - The query string to use for this definition. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax> .
putQueryDefinition ::
  -- | 'pqdName'
  Text ->
  -- | 'pqdQueryString'
  Text ->
  PutQueryDefinition
putQueryDefinition pName_ pQueryString_ =
  PutQueryDefinition'
    { _pqdLogGroupNames = Nothing,
      _pqdQueryDefinitionId = Nothing,
      _pqdName = pName_,
      _pqdQueryString = pQueryString_
    }

-- | Use this parameter to include specific log groups as part of your query definition. If you are updating a query definition and you omit this parameter, then the updated definition will contain no log groups.
pqdLogGroupNames :: Lens' PutQueryDefinition [Text]
pqdLogGroupNames = lens _pqdLogGroupNames (\s a -> s {_pqdLogGroupNames = a}) . _Default . _Coerce

-- | If you are updating a query definition, use this parameter to specify the ID of the query definition that you want to update. You can use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions> to retrieve the IDs of your saved query definitions. If you are creating a query definition, do not specify this parameter. CloudWatch generates a unique ID for the new query definition and include it in the response to this operation.
pqdQueryDefinitionId :: Lens' PutQueryDefinition (Maybe Text)
pqdQueryDefinitionId = lens _pqdQueryDefinitionId (\s a -> s {_pqdQueryDefinitionId = a})

-- | A name for the query definition. If you are saving a lot of query definitions, we recommend that you name them so that you can easily find the ones you want by using the first part of the name as a filter in the @queryDefinitionNamePrefix@ parameter of <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions> .
pqdName :: Lens' PutQueryDefinition Text
pqdName = lens _pqdName (\s a -> s {_pqdName = a})

-- | The query string to use for this definition. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax> .
pqdQueryString :: Lens' PutQueryDefinition Text
pqdQueryString = lens _pqdQueryString (\s a -> s {_pqdQueryString = a})

instance AWSRequest PutQueryDefinition where
  type Rs PutQueryDefinition = PutQueryDefinitionResponse
  request = postJSON cloudWatchLogs
  response =
    receiveJSON
      ( \s h x ->
          PutQueryDefinitionResponse'
            <$> (x .?> "queryDefinitionId") <*> (pure (fromEnum s))
      )

instance Hashable PutQueryDefinition

instance NFData PutQueryDefinition

instance ToHeaders PutQueryDefinition where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Logs_20140328.PutQueryDefinition" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutQueryDefinition where
  toJSON PutQueryDefinition' {..} =
    object
      ( catMaybes
          [ ("logGroupNames" .=) <$> _pqdLogGroupNames,
            ("queryDefinitionId" .=) <$> _pqdQueryDefinitionId,
            Just ("name" .= _pqdName),
            Just ("queryString" .= _pqdQueryString)
          ]
      )

instance ToPath PutQueryDefinition where
  toPath = const "/"

instance ToQuery PutQueryDefinition where
  toQuery = const mempty

-- | /See:/ 'putQueryDefinitionResponse' smart constructor.
data PutQueryDefinitionResponse = PutQueryDefinitionResponse'
  { _pqdrsQueryDefinitionId ::
      !(Maybe Text),
    _pqdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutQueryDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pqdrsQueryDefinitionId' - The ID of the query definition.
--
-- * 'pqdrsResponseStatus' - -- | The response status code.
putQueryDefinitionResponse ::
  -- | 'pqdrsResponseStatus'
  Int ->
  PutQueryDefinitionResponse
putQueryDefinitionResponse pResponseStatus_ =
  PutQueryDefinitionResponse'
    { _pqdrsQueryDefinitionId = Nothing,
      _pqdrsResponseStatus = pResponseStatus_
    }

-- | The ID of the query definition.
pqdrsQueryDefinitionId :: Lens' PutQueryDefinitionResponse (Maybe Text)
pqdrsQueryDefinitionId = lens _pqdrsQueryDefinitionId (\s a -> s {_pqdrsQueryDefinitionId = a})

-- | -- | The response status code.
pqdrsResponseStatus :: Lens' PutQueryDefinitionResponse Int
pqdrsResponseStatus = lens _pqdrsResponseStatus (\s a -> s {_pqdrsResponseStatus = a})

instance NFData PutQueryDefinitionResponse
