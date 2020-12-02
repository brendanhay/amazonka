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
-- Module      : Network.AWS.Config.SelectResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a structured query language (SQL) @SELECT@ command, performs the corresponding search, and returns resource configurations matching the properties.
--
--
-- For more information about query components, see the <https://docs.aws.amazon.com/config/latest/developerguide/query-components.html __Query Components__ > section in the AWS Config Developer Guide.
module Network.AWS.Config.SelectResourceConfig
  ( -- * Creating a Request
    selectResourceConfig,
    SelectResourceConfig,

    -- * Request Lenses
    srcNextToken,
    srcLimit,
    srcExpression,

    -- * Destructuring the Response
    selectResourceConfigResponse,
    SelectResourceConfigResponse,

    -- * Response Lenses
    srcrsResults,
    srcrsQueryInfo,
    srcrsNextToken,
    srcrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'selectResourceConfig' smart constructor.
data SelectResourceConfig = SelectResourceConfig'
  { _srcNextToken ::
      !(Maybe Text),
    _srcLimit :: !(Maybe Nat),
    _srcExpression :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SelectResourceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srcNextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- * 'srcLimit' - The maximum number of query results returned on each page.
--
-- * 'srcExpression' - The SQL query @SELECT@ command.
selectResourceConfig ::
  -- | 'srcExpression'
  Text ->
  SelectResourceConfig
selectResourceConfig pExpression_ =
  SelectResourceConfig'
    { _srcNextToken = Nothing,
      _srcLimit = Nothing,
      _srcExpression = pExpression_
    }

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
srcNextToken :: Lens' SelectResourceConfig (Maybe Text)
srcNextToken = lens _srcNextToken (\s a -> s {_srcNextToken = a})

-- | The maximum number of query results returned on each page.
srcLimit :: Lens' SelectResourceConfig (Maybe Natural)
srcLimit = lens _srcLimit (\s a -> s {_srcLimit = a}) . mapping _Nat

-- | The SQL query @SELECT@ command.
srcExpression :: Lens' SelectResourceConfig Text
srcExpression = lens _srcExpression (\s a -> s {_srcExpression = a})

instance AWSRequest SelectResourceConfig where
  type Rs SelectResourceConfig = SelectResourceConfigResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          SelectResourceConfigResponse'
            <$> (x .?> "Results" .!@ mempty)
            <*> (x .?> "QueryInfo")
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable SelectResourceConfig

instance NFData SelectResourceConfig

instance ToHeaders SelectResourceConfig where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StarlingDoveService.SelectResourceConfig" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON SelectResourceConfig where
  toJSON SelectResourceConfig' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _srcNextToken,
            ("Limit" .=) <$> _srcLimit,
            Just ("Expression" .= _srcExpression)
          ]
      )

instance ToPath SelectResourceConfig where
  toPath = const "/"

instance ToQuery SelectResourceConfig where
  toQuery = const mempty

-- | /See:/ 'selectResourceConfigResponse' smart constructor.
data SelectResourceConfigResponse = SelectResourceConfigResponse'
  { _srcrsResults ::
      !(Maybe [Text]),
    _srcrsQueryInfo ::
      !(Maybe QueryInfo),
    _srcrsNextToken :: !(Maybe Text),
    _srcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SelectResourceConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srcrsResults' - Returns the results for the SQL query.
--
-- * 'srcrsQueryInfo' - Returns the @QueryInfo@ object.
--
-- * 'srcrsNextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- * 'srcrsResponseStatus' - -- | The response status code.
selectResourceConfigResponse ::
  -- | 'srcrsResponseStatus'
  Int ->
  SelectResourceConfigResponse
selectResourceConfigResponse pResponseStatus_ =
  SelectResourceConfigResponse'
    { _srcrsResults = Nothing,
      _srcrsQueryInfo = Nothing,
      _srcrsNextToken = Nothing,
      _srcrsResponseStatus = pResponseStatus_
    }

-- | Returns the results for the SQL query.
srcrsResults :: Lens' SelectResourceConfigResponse [Text]
srcrsResults = lens _srcrsResults (\s a -> s {_srcrsResults = a}) . _Default . _Coerce

-- | Returns the @QueryInfo@ object.
srcrsQueryInfo :: Lens' SelectResourceConfigResponse (Maybe QueryInfo)
srcrsQueryInfo = lens _srcrsQueryInfo (\s a -> s {_srcrsQueryInfo = a})

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
srcrsNextToken :: Lens' SelectResourceConfigResponse (Maybe Text)
srcrsNextToken = lens _srcrsNextToken (\s a -> s {_srcrsNextToken = a})

-- | -- | The response status code.
srcrsResponseStatus :: Lens' SelectResourceConfigResponse Int
srcrsResponseStatus = lens _srcrsResponseStatus (\s a -> s {_srcrsResponseStatus = a})

instance NFData SelectResourceConfigResponse
