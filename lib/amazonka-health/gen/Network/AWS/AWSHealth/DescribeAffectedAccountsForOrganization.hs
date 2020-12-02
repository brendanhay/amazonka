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
-- Module      : Network.AWS.AWSHealth.DescribeAffectedAccountsForOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of accounts in the organization from AWS Organizations that are affected by the provided event. For more information about the different types of AWS Health events, see <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event> .
--
--
-- Before you can call this operation, you must first enable AWS Health to work with AWS Organizations. To do this, call the <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization> operation from your organization's master account.
--
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeAffectedAccountsForOrganization
  ( -- * Creating a Request
    describeAffectedAccountsForOrganization,
    DescribeAffectedAccountsForOrganization,

    -- * Request Lenses
    daafoNextToken,
    daafoMaxResults,
    daafoEventARN,

    -- * Destructuring the Response
    describeAffectedAccountsForOrganizationResponse,
    DescribeAffectedAccountsForOrganizationResponse,

    -- * Response Lenses
    daaforsAffectedAccounts,
    daaforsEventScopeCode,
    daaforsNextToken,
    daaforsResponseStatus,
  )
where

import Network.AWS.AWSHealth.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAffectedAccountsForOrganization' smart constructor.
data DescribeAffectedAccountsForOrganization = DescribeAffectedAccountsForOrganization'
  { _daafoNextToken ::
      !( Maybe
           Text
       ),
    _daafoMaxResults ::
      !( Maybe
           Nat
       ),
    _daafoEventARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAffectedAccountsForOrganization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daafoNextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- * 'daafoMaxResults' - The maximum number of items to return in one batch, between 10 and 100, inclusive.
--
-- * 'daafoEventARN' - The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
describeAffectedAccountsForOrganization ::
  -- | 'daafoEventARN'
  Text ->
  DescribeAffectedAccountsForOrganization
describeAffectedAccountsForOrganization pEventARN_ =
  DescribeAffectedAccountsForOrganization'
    { _daafoNextToken =
        Nothing,
      _daafoMaxResults = Nothing,
      _daafoEventARN = pEventARN_
    }

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
daafoNextToken :: Lens' DescribeAffectedAccountsForOrganization (Maybe Text)
daafoNextToken = lens _daafoNextToken (\s a -> s {_daafoNextToken = a})

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
daafoMaxResults :: Lens' DescribeAffectedAccountsForOrganization (Maybe Natural)
daafoMaxResults = lens _daafoMaxResults (\s a -> s {_daafoMaxResults = a}) . mapping _Nat

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
daafoEventARN :: Lens' DescribeAffectedAccountsForOrganization Text
daafoEventARN = lens _daafoEventARN (\s a -> s {_daafoEventARN = a})

instance AWSPager DescribeAffectedAccountsForOrganization where
  page rq rs
    | stop (rs ^. daaforsNextToken) = Nothing
    | stop (rs ^. daaforsAffectedAccounts) = Nothing
    | otherwise = Just $ rq & daafoNextToken .~ rs ^. daaforsNextToken

instance AWSRequest DescribeAffectedAccountsForOrganization where
  type
    Rs DescribeAffectedAccountsForOrganization =
      DescribeAffectedAccountsForOrganizationResponse
  request = postJSON awsHealth
  response =
    receiveJSON
      ( \s h x ->
          DescribeAffectedAccountsForOrganizationResponse'
            <$> (x .?> "affectedAccounts" .!@ mempty)
            <*> (x .?> "eventScopeCode")
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeAffectedAccountsForOrganization

instance NFData DescribeAffectedAccountsForOrganization

instance ToHeaders DescribeAffectedAccountsForOrganization where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSHealth_20160804.DescribeAffectedAccountsForOrganization" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeAffectedAccountsForOrganization where
  toJSON DescribeAffectedAccountsForOrganization' {..} =
    object
      ( catMaybes
          [ ("nextToken" .=) <$> _daafoNextToken,
            ("maxResults" .=) <$> _daafoMaxResults,
            Just ("eventArn" .= _daafoEventARN)
          ]
      )

instance ToPath DescribeAffectedAccountsForOrganization where
  toPath = const "/"

instance ToQuery DescribeAffectedAccountsForOrganization where
  toQuery = const mempty

-- | /See:/ 'describeAffectedAccountsForOrganizationResponse' smart constructor.
data DescribeAffectedAccountsForOrganizationResponse = DescribeAffectedAccountsForOrganizationResponse'
  { _daaforsAffectedAccounts ::
      !( Maybe
           [Text]
       ),
    _daaforsEventScopeCode ::
      !( Maybe
           EventScopeCode
       ),
    _daaforsNextToken ::
      !( Maybe
           Text
       ),
    _daaforsResponseStatus ::
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

-- | Creates a value of 'DescribeAffectedAccountsForOrganizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daaforsAffectedAccounts' - A JSON set of elements of the affected accounts.
--
-- * 'daaforsEventScopeCode' - This parameter specifies if the AWS Health event is a public AWS service event or an account-specific event.     * If the @eventScopeCode@ value is @PUBLIC@ , then the @affectedAccounts@ value is always empty.     * If the @eventScopeCode@ value is @ACCOUNT_SPECIFIC@ , then the @affectedAccounts@ value lists the affected AWS accounts in your organization. For example, if an event affects a service such as Amazon Elastic Compute Cloud and you have AWS accounts that use that service, those account IDs appear in the response.     * If the @eventScopeCode@ value is @NONE@ , then the @eventArn@ that you specified in the request is invalid or doesn't exist.
--
-- * 'daaforsNextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- * 'daaforsResponseStatus' - -- | The response status code.
describeAffectedAccountsForOrganizationResponse ::
  -- | 'daaforsResponseStatus'
  Int ->
  DescribeAffectedAccountsForOrganizationResponse
describeAffectedAccountsForOrganizationResponse pResponseStatus_ =
  DescribeAffectedAccountsForOrganizationResponse'
    { _daaforsAffectedAccounts =
        Nothing,
      _daaforsEventScopeCode = Nothing,
      _daaforsNextToken = Nothing,
      _daaforsResponseStatus = pResponseStatus_
    }

-- | A JSON set of elements of the affected accounts.
daaforsAffectedAccounts :: Lens' DescribeAffectedAccountsForOrganizationResponse [Text]
daaforsAffectedAccounts = lens _daaforsAffectedAccounts (\s a -> s {_daaforsAffectedAccounts = a}) . _Default . _Coerce

-- | This parameter specifies if the AWS Health event is a public AWS service event or an account-specific event.     * If the @eventScopeCode@ value is @PUBLIC@ , then the @affectedAccounts@ value is always empty.     * If the @eventScopeCode@ value is @ACCOUNT_SPECIFIC@ , then the @affectedAccounts@ value lists the affected AWS accounts in your organization. For example, if an event affects a service such as Amazon Elastic Compute Cloud and you have AWS accounts that use that service, those account IDs appear in the response.     * If the @eventScopeCode@ value is @NONE@ , then the @eventArn@ that you specified in the request is invalid or doesn't exist.
daaforsEventScopeCode :: Lens' DescribeAffectedAccountsForOrganizationResponse (Maybe EventScopeCode)
daaforsEventScopeCode = lens _daaforsEventScopeCode (\s a -> s {_daaforsEventScopeCode = a})

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
daaforsNextToken :: Lens' DescribeAffectedAccountsForOrganizationResponse (Maybe Text)
daaforsNextToken = lens _daaforsNextToken (\s a -> s {_daaforsNextToken = a})

-- | -- | The response status code.
daaforsResponseStatus :: Lens' DescribeAffectedAccountsForOrganizationResponse Int
daaforsResponseStatus = lens _daaforsResponseStatus (\s a -> s {_daaforsResponseStatus = a})

instance NFData DescribeAffectedAccountsForOrganizationResponse
