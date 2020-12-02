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
-- Module      : Network.AWS.DynamoDB.DescribeContributorInsights
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about contributor insights, for a given table or global secondary index.
module Network.AWS.DynamoDB.DescribeContributorInsights
  ( -- * Creating a Request
    describeContributorInsights,
    DescribeContributorInsights,

    -- * Request Lenses
    dciIndexName,
    dciTableName,

    -- * Destructuring the Response
    describeContributorInsightsResponse,
    DescribeContributorInsightsResponse,

    -- * Response Lenses
    dcirsContributorInsightsRuleList,
    dcirsFailureException,
    dcirsContributorInsightsStatus,
    dcirsLastUpdateDateTime,
    dcirsTableName,
    dcirsIndexName,
    dcirsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeContributorInsights' smart constructor.
data DescribeContributorInsights = DescribeContributorInsights'
  { _dciIndexName ::
      !(Maybe Text),
    _dciTableName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeContributorInsights' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dciIndexName' - The name of the global secondary index to describe, if applicable.
--
-- * 'dciTableName' - The name of the table to describe.
describeContributorInsights ::
  -- | 'dciTableName'
  Text ->
  DescribeContributorInsights
describeContributorInsights pTableName_ =
  DescribeContributorInsights'
    { _dciIndexName = Nothing,
      _dciTableName = pTableName_
    }

-- | The name of the global secondary index to describe, if applicable.
dciIndexName :: Lens' DescribeContributorInsights (Maybe Text)
dciIndexName = lens _dciIndexName (\s a -> s {_dciIndexName = a})

-- | The name of the table to describe.
dciTableName :: Lens' DescribeContributorInsights Text
dciTableName = lens _dciTableName (\s a -> s {_dciTableName = a})

instance AWSRequest DescribeContributorInsights where
  type
    Rs DescribeContributorInsights =
      DescribeContributorInsightsResponse
  request = postJSON dynamoDB
  response =
    receiveJSON
      ( \s h x ->
          DescribeContributorInsightsResponse'
            <$> (x .?> "ContributorInsightsRuleList" .!@ mempty)
            <*> (x .?> "FailureException")
            <*> (x .?> "ContributorInsightsStatus")
            <*> (x .?> "LastUpdateDateTime")
            <*> (x .?> "TableName")
            <*> (x .?> "IndexName")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeContributorInsights

instance NFData DescribeContributorInsights

instance ToHeaders DescribeContributorInsights where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DynamoDB_20120810.DescribeContributorInsights" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON DescribeContributorInsights where
  toJSON DescribeContributorInsights' {..} =
    object
      ( catMaybes
          [ ("IndexName" .=) <$> _dciIndexName,
            Just ("TableName" .= _dciTableName)
          ]
      )

instance ToPath DescribeContributorInsights where
  toPath = const "/"

instance ToQuery DescribeContributorInsights where
  toQuery = const mempty

-- | /See:/ 'describeContributorInsightsResponse' smart constructor.
data DescribeContributorInsightsResponse = DescribeContributorInsightsResponse'
  { _dcirsContributorInsightsRuleList ::
      !(Maybe [Text]),
    _dcirsFailureException ::
      !( Maybe
           FailureException
       ),
    _dcirsContributorInsightsStatus ::
      !( Maybe
           ContributorInsightsStatus
       ),
    _dcirsLastUpdateDateTime ::
      !(Maybe POSIX),
    _dcirsTableName ::
      !(Maybe Text),
    _dcirsIndexName ::
      !(Maybe Text),
    _dcirsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeContributorInsightsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcirsContributorInsightsRuleList' - List of names of the associated Alpine rules.
--
-- * 'dcirsFailureException' - Returns information about the last failure that encountered. The most common exceptions for a FAILED status are:     * LimitExceededException - Per-account Amazon CloudWatch Contributor Insights rule limit reached. Please disable Contributor Insights for other tables/indexes OR disable Contributor Insights rules before retrying.     * AccessDeniedException - Amazon CloudWatch Contributor Insights rules cannot be modified due to insufficient permissions.     * AccessDeniedException - Failed to create service-linked role for Contributor Insights due to insufficient permissions.     * InternalServerError - Failed to create Amazon CloudWatch Contributor Insights rules. Please retry request.
--
-- * 'dcirsContributorInsightsStatus' - Current Status contributor insights.
--
-- * 'dcirsLastUpdateDateTime' - Timestamp of the last time the status was changed.
--
-- * 'dcirsTableName' - The name of the table being described.
--
-- * 'dcirsIndexName' - The name of the global secondary index being described.
--
-- * 'dcirsResponseStatus' - -- | The response status code.
describeContributorInsightsResponse ::
  -- | 'dcirsResponseStatus'
  Int ->
  DescribeContributorInsightsResponse
describeContributorInsightsResponse pResponseStatus_ =
  DescribeContributorInsightsResponse'
    { _dcirsContributorInsightsRuleList =
        Nothing,
      _dcirsFailureException = Nothing,
      _dcirsContributorInsightsStatus = Nothing,
      _dcirsLastUpdateDateTime = Nothing,
      _dcirsTableName = Nothing,
      _dcirsIndexName = Nothing,
      _dcirsResponseStatus = pResponseStatus_
    }

-- | List of names of the associated Alpine rules.
dcirsContributorInsightsRuleList :: Lens' DescribeContributorInsightsResponse [Text]
dcirsContributorInsightsRuleList = lens _dcirsContributorInsightsRuleList (\s a -> s {_dcirsContributorInsightsRuleList = a}) . _Default . _Coerce

-- | Returns information about the last failure that encountered. The most common exceptions for a FAILED status are:     * LimitExceededException - Per-account Amazon CloudWatch Contributor Insights rule limit reached. Please disable Contributor Insights for other tables/indexes OR disable Contributor Insights rules before retrying.     * AccessDeniedException - Amazon CloudWatch Contributor Insights rules cannot be modified due to insufficient permissions.     * AccessDeniedException - Failed to create service-linked role for Contributor Insights due to insufficient permissions.     * InternalServerError - Failed to create Amazon CloudWatch Contributor Insights rules. Please retry request.
dcirsFailureException :: Lens' DescribeContributorInsightsResponse (Maybe FailureException)
dcirsFailureException = lens _dcirsFailureException (\s a -> s {_dcirsFailureException = a})

-- | Current Status contributor insights.
dcirsContributorInsightsStatus :: Lens' DescribeContributorInsightsResponse (Maybe ContributorInsightsStatus)
dcirsContributorInsightsStatus = lens _dcirsContributorInsightsStatus (\s a -> s {_dcirsContributorInsightsStatus = a})

-- | Timestamp of the last time the status was changed.
dcirsLastUpdateDateTime :: Lens' DescribeContributorInsightsResponse (Maybe UTCTime)
dcirsLastUpdateDateTime = lens _dcirsLastUpdateDateTime (\s a -> s {_dcirsLastUpdateDateTime = a}) . mapping _Time

-- | The name of the table being described.
dcirsTableName :: Lens' DescribeContributorInsightsResponse (Maybe Text)
dcirsTableName = lens _dcirsTableName (\s a -> s {_dcirsTableName = a})

-- | The name of the global secondary index being described.
dcirsIndexName :: Lens' DescribeContributorInsightsResponse (Maybe Text)
dcirsIndexName = lens _dcirsIndexName (\s a -> s {_dcirsIndexName = a})

-- | -- | The response status code.
dcirsResponseStatus :: Lens' DescribeContributorInsightsResponse Int
dcirsResponseStatus = lens _dcirsResponseStatus (\s a -> s {_dcirsResponseStatus = a})

instance NFData DescribeContributorInsightsResponse
