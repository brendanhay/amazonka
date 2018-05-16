{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.ListAttacks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all ongoing DDoS attacks or all DDoS attacks during a specified time period.
--
--
module Network.AWS.Shield.ListAttacks
    (
    -- * Creating a Request
      listAttacks
    , ListAttacks
    -- * Request Lenses
    , laStartTime
    , laResourceARNs
    , laNextToken
    , laEndTime
    , laMaxResults

    -- * Destructuring the Response
    , listAttacksResponse
    , ListAttacksResponse
    -- * Response Lenses
    , larsAttackSummaries
    , larsNextToken
    , larsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types
import Network.AWS.Shield.Types.Product

-- | /See:/ 'listAttacks' smart constructor.
data ListAttacks = ListAttacks'
  { _laStartTime    :: !(Maybe TimeRange)
  , _laResourceARNs :: !(Maybe [Text])
  , _laNextToken    :: !(Maybe Text)
  , _laEndTime      :: !(Maybe TimeRange)
  , _laMaxResults   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAttacks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laStartTime' - The start of the time period for the attacks. This is a @timestamp@ type. The sample request above indicates a @number@ type because the default used by WAF is Unix time in seconds. However any valid <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format> is allowed.
--
-- * 'laResourceARNs' - The ARN (Amazon Resource Name) of the resource that was attacked. If this is left blank, all applicable resources for this account will be included.
--
-- * 'laNextToken' - The @ListAttacksRequest.NextMarker@ value from a previous call to @ListAttacksRequest@ . Pass null if this is the first call.
--
-- * 'laEndTime' - The end of the time period for the attacks. This is a @timestamp@ type. The sample request above indicates a @number@ type because the default used by WAF is Unix time in seconds. However any valid <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format> is allowed.
--
-- * 'laMaxResults' - The maximum number of 'AttackSummary' objects to be returned. If this is left blank, the first 20 results will be returned.
listAttacks
    :: ListAttacks
listAttacks =
  ListAttacks'
    { _laStartTime = Nothing
    , _laResourceARNs = Nothing
    , _laNextToken = Nothing
    , _laEndTime = Nothing
    , _laMaxResults = Nothing
    }


-- | The start of the time period for the attacks. This is a @timestamp@ type. The sample request above indicates a @number@ type because the default used by WAF is Unix time in seconds. However any valid <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format> is allowed.
laStartTime :: Lens' ListAttacks (Maybe TimeRange)
laStartTime = lens _laStartTime (\ s a -> s{_laStartTime = a})

-- | The ARN (Amazon Resource Name) of the resource that was attacked. If this is left blank, all applicable resources for this account will be included.
laResourceARNs :: Lens' ListAttacks [Text]
laResourceARNs = lens _laResourceARNs (\ s a -> s{_laResourceARNs = a}) . _Default . _Coerce

-- | The @ListAttacksRequest.NextMarker@ value from a previous call to @ListAttacksRequest@ . Pass null if this is the first call.
laNextToken :: Lens' ListAttacks (Maybe Text)
laNextToken = lens _laNextToken (\ s a -> s{_laNextToken = a})

-- | The end of the time period for the attacks. This is a @timestamp@ type. The sample request above indicates a @number@ type because the default used by WAF is Unix time in seconds. However any valid <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format> is allowed.
laEndTime :: Lens' ListAttacks (Maybe TimeRange)
laEndTime = lens _laEndTime (\ s a -> s{_laEndTime = a})

-- | The maximum number of 'AttackSummary' objects to be returned. If this is left blank, the first 20 results will be returned.
laMaxResults :: Lens' ListAttacks (Maybe Natural)
laMaxResults = lens _laMaxResults (\ s a -> s{_laMaxResults = a}) . mapping _Nat

instance AWSRequest ListAttacks where
        type Rs ListAttacks = ListAttacksResponse
        request = postJSON shield
        response
          = receiveJSON
              (\ s h x ->
                 ListAttacksResponse' <$>
                   (x .?> "AttackSummaries" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListAttacks where

instance NFData ListAttacks where

instance ToHeaders ListAttacks where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSShield_20160616.ListAttacks" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAttacks where
        toJSON ListAttacks'{..}
          = object
              (catMaybes
                 [("StartTime" .=) <$> _laStartTime,
                  ("ResourceArns" .=) <$> _laResourceARNs,
                  ("NextToken" .=) <$> _laNextToken,
                  ("EndTime" .=) <$> _laEndTime,
                  ("MaxResults" .=) <$> _laMaxResults])

instance ToPath ListAttacks where
        toPath = const "/"

instance ToQuery ListAttacks where
        toQuery = const mempty

-- | /See:/ 'listAttacksResponse' smart constructor.
data ListAttacksResponse = ListAttacksResponse'
  { _larsAttackSummaries :: !(Maybe [AttackSummary])
  , _larsNextToken       :: !(Maybe Text)
  , _larsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAttacksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsAttackSummaries' - The attack information for the specified time range.
--
-- * 'larsNextToken' - The token returned by a previous call to indicate that there is more data available. If not null, more results are available. Pass this value for the @NextMarker@ parameter in a subsequent call to @ListAttacks@ to retrieve the next set of items.
--
-- * 'larsResponseStatus' - -- | The response status code.
listAttacksResponse
    :: Int -- ^ 'larsResponseStatus'
    -> ListAttacksResponse
listAttacksResponse pResponseStatus_ =
  ListAttacksResponse'
    { _larsAttackSummaries = Nothing
    , _larsNextToken = Nothing
    , _larsResponseStatus = pResponseStatus_
    }


-- | The attack information for the specified time range.
larsAttackSummaries :: Lens' ListAttacksResponse [AttackSummary]
larsAttackSummaries = lens _larsAttackSummaries (\ s a -> s{_larsAttackSummaries = a}) . _Default . _Coerce

-- | The token returned by a previous call to indicate that there is more data available. If not null, more results are available. Pass this value for the @NextMarker@ parameter in a subsequent call to @ListAttacks@ to retrieve the next set of items.
larsNextToken :: Lens' ListAttacksResponse (Maybe Text)
larsNextToken = lens _larsNextToken (\ s a -> s{_larsNextToken = a})

-- | -- | The response status code.
larsResponseStatus :: Lens' ListAttacksResponse Int
larsResponseStatus = lens _larsResponseStatus (\ s a -> s{_larsResponseStatus = a})

instance NFData ListAttacksResponse where
