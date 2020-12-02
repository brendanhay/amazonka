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
-- Module      : Network.AWS.Config.DescribeRemediationExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a detailed view of a Remediation Execution for a set of resources including state, timestamps for when steps for the remediation execution occur, and any error messages for steps that have failed. When you specify the limit and the next token, you receive a paginated response.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeRemediationExecutionStatus
  ( -- * Creating a Request
    describeRemediationExecutionStatus,
    DescribeRemediationExecutionStatus,

    -- * Request Lenses
    dresNextToken,
    dresLimit,
    dresResourceKeys,
    dresConfigRuleName,

    -- * Destructuring the Response
    describeRemediationExecutionStatusResponse,
    DescribeRemediationExecutionStatusResponse,

    -- * Response Lenses
    dresrsRemediationExecutionStatuses,
    dresrsNextToken,
    dresrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeRemediationExecutionStatus' smart constructor.
data DescribeRemediationExecutionStatus = DescribeRemediationExecutionStatus'
  { _dresNextToken ::
      !(Maybe Text),
    _dresLimit ::
      !(Maybe Nat),
    _dresResourceKeys ::
      !( Maybe
           ( List1
               ResourceKey
           )
       ),
    _dresConfigRuleName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeRemediationExecutionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dresNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'dresLimit' - The maximum number of RemediationExecutionStatuses returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
--
-- * 'dresResourceKeys' - A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
--
-- * 'dresConfigRuleName' - A list of AWS Config rule names.
describeRemediationExecutionStatus ::
  -- | 'dresConfigRuleName'
  Text ->
  DescribeRemediationExecutionStatus
describeRemediationExecutionStatus pConfigRuleName_ =
  DescribeRemediationExecutionStatus'
    { _dresNextToken = Nothing,
      _dresLimit = Nothing,
      _dresResourceKeys = Nothing,
      _dresConfigRuleName = pConfigRuleName_
    }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
dresNextToken :: Lens' DescribeRemediationExecutionStatus (Maybe Text)
dresNextToken = lens _dresNextToken (\s a -> s {_dresNextToken = a})

-- | The maximum number of RemediationExecutionStatuses returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
dresLimit :: Lens' DescribeRemediationExecutionStatus (Maybe Natural)
dresLimit = lens _dresLimit (\s a -> s {_dresLimit = a}) . mapping _Nat

-- | A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
dresResourceKeys :: Lens' DescribeRemediationExecutionStatus (Maybe (NonEmpty ResourceKey))
dresResourceKeys = lens _dresResourceKeys (\s a -> s {_dresResourceKeys = a}) . mapping _List1

-- | A list of AWS Config rule names.
dresConfigRuleName :: Lens' DescribeRemediationExecutionStatus Text
dresConfigRuleName = lens _dresConfigRuleName (\s a -> s {_dresConfigRuleName = a})

instance AWSPager DescribeRemediationExecutionStatus where
  page rq rs
    | stop (rs ^. dresrsNextToken) = Nothing
    | stop (rs ^. dresrsRemediationExecutionStatuses) = Nothing
    | otherwise = Just $ rq & dresNextToken .~ rs ^. dresrsNextToken

instance AWSRequest DescribeRemediationExecutionStatus where
  type
    Rs DescribeRemediationExecutionStatus =
      DescribeRemediationExecutionStatusResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          DescribeRemediationExecutionStatusResponse'
            <$> (x .?> "RemediationExecutionStatuses" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeRemediationExecutionStatus

instance NFData DescribeRemediationExecutionStatus

instance ToHeaders DescribeRemediationExecutionStatus where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StarlingDoveService.DescribeRemediationExecutionStatus" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeRemediationExecutionStatus where
  toJSON DescribeRemediationExecutionStatus' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _dresNextToken,
            ("Limit" .=) <$> _dresLimit,
            ("ResourceKeys" .=) <$> _dresResourceKeys,
            Just ("ConfigRuleName" .= _dresConfigRuleName)
          ]
      )

instance ToPath DescribeRemediationExecutionStatus where
  toPath = const "/"

instance ToQuery DescribeRemediationExecutionStatus where
  toQuery = const mempty

-- | /See:/ 'describeRemediationExecutionStatusResponse' smart constructor.
data DescribeRemediationExecutionStatusResponse = DescribeRemediationExecutionStatusResponse'
  { _dresrsRemediationExecutionStatuses ::
      !( Maybe
           [RemediationExecutionStatus]
       ),
    _dresrsNextToken ::
      !( Maybe
           Text
       ),
    _dresrsResponseStatus ::
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

-- | Creates a value of 'DescribeRemediationExecutionStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dresrsRemediationExecutionStatuses' - Returns a list of remediation execution statuses objects.
--
-- * 'dresrsNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'dresrsResponseStatus' - -- | The response status code.
describeRemediationExecutionStatusResponse ::
  -- | 'dresrsResponseStatus'
  Int ->
  DescribeRemediationExecutionStatusResponse
describeRemediationExecutionStatusResponse pResponseStatus_ =
  DescribeRemediationExecutionStatusResponse'
    { _dresrsRemediationExecutionStatuses =
        Nothing,
      _dresrsNextToken = Nothing,
      _dresrsResponseStatus = pResponseStatus_
    }

-- | Returns a list of remediation execution statuses objects.
dresrsRemediationExecutionStatuses :: Lens' DescribeRemediationExecutionStatusResponse [RemediationExecutionStatus]
dresrsRemediationExecutionStatuses = lens _dresrsRemediationExecutionStatuses (\s a -> s {_dresrsRemediationExecutionStatuses = a}) . _Default . _Coerce

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
dresrsNextToken :: Lens' DescribeRemediationExecutionStatusResponse (Maybe Text)
dresrsNextToken = lens _dresrsNextToken (\s a -> s {_dresrsNextToken = a})

-- | -- | The response status code.
dresrsResponseStatus :: Lens' DescribeRemediationExecutionStatusResponse Int
dresrsResponseStatus = lens _dresrsResponseStatus (\s a -> s {_dresrsResponseStatus = a})

instance NFData DescribeRemediationExecutionStatusResponse
