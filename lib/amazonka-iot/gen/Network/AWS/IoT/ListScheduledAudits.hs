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
-- Module      : Network.AWS.IoT.ListScheduledAudits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of your scheduled audits.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListScheduledAudits
  ( -- * Creating a Request
    listScheduledAudits,
    ListScheduledAudits,

    -- * Request Lenses
    lsaNextToken,
    lsaMaxResults,

    -- * Destructuring the Response
    listScheduledAuditsResponse,
    ListScheduledAuditsResponse,

    -- * Response Lenses
    lsarsScheduledAudits,
    lsarsNextToken,
    lsarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listScheduledAudits' smart constructor.
data ListScheduledAudits = ListScheduledAudits'
  { _lsaNextToken ::
      !(Maybe Text),
    _lsaMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListScheduledAudits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsaNextToken' - The token for the next set of results.
--
-- * 'lsaMaxResults' - The maximum number of results to return at one time. The default is 25.
listScheduledAudits ::
  ListScheduledAudits
listScheduledAudits =
  ListScheduledAudits'
    { _lsaNextToken = Nothing,
      _lsaMaxResults = Nothing
    }

-- | The token for the next set of results.
lsaNextToken :: Lens' ListScheduledAudits (Maybe Text)
lsaNextToken = lens _lsaNextToken (\s a -> s {_lsaNextToken = a})

-- | The maximum number of results to return at one time. The default is 25.
lsaMaxResults :: Lens' ListScheduledAudits (Maybe Natural)
lsaMaxResults = lens _lsaMaxResults (\s a -> s {_lsaMaxResults = a}) . mapping _Nat

instance AWSPager ListScheduledAudits where
  page rq rs
    | stop (rs ^. lsarsNextToken) = Nothing
    | stop (rs ^. lsarsScheduledAudits) = Nothing
    | otherwise = Just $ rq & lsaNextToken .~ rs ^. lsarsNextToken

instance AWSRequest ListScheduledAudits where
  type Rs ListScheduledAudits = ListScheduledAuditsResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          ListScheduledAuditsResponse'
            <$> (x .?> "scheduledAudits" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListScheduledAudits

instance NFData ListScheduledAudits

instance ToHeaders ListScheduledAudits where
  toHeaders = const mempty

instance ToPath ListScheduledAudits where
  toPath = const "/audit/scheduledaudits"

instance ToQuery ListScheduledAudits where
  toQuery ListScheduledAudits' {..} =
    mconcat
      ["nextToken" =: _lsaNextToken, "maxResults" =: _lsaMaxResults]

-- | /See:/ 'listScheduledAuditsResponse' smart constructor.
data ListScheduledAuditsResponse = ListScheduledAuditsResponse'
  { _lsarsScheduledAudits ::
      !(Maybe [ScheduledAuditMetadata]),
    _lsarsNextToken :: !(Maybe Text),
    _lsarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListScheduledAuditsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsarsScheduledAudits' - The list of scheduled audits.
--
-- * 'lsarsNextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- * 'lsarsResponseStatus' - -- | The response status code.
listScheduledAuditsResponse ::
  -- | 'lsarsResponseStatus'
  Int ->
  ListScheduledAuditsResponse
listScheduledAuditsResponse pResponseStatus_ =
  ListScheduledAuditsResponse'
    { _lsarsScheduledAudits = Nothing,
      _lsarsNextToken = Nothing,
      _lsarsResponseStatus = pResponseStatus_
    }

-- | The list of scheduled audits.
lsarsScheduledAudits :: Lens' ListScheduledAuditsResponse [ScheduledAuditMetadata]
lsarsScheduledAudits = lens _lsarsScheduledAudits (\s a -> s {_lsarsScheduledAudits = a}) . _Default . _Coerce

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
lsarsNextToken :: Lens' ListScheduledAuditsResponse (Maybe Text)
lsarsNextToken = lens _lsarsNextToken (\s a -> s {_lsarsNextToken = a})

-- | -- | The response status code.
lsarsResponseStatus :: Lens' ListScheduledAuditsResponse Int
lsarsResponseStatus = lens _lsarsResponseStatus (\s a -> s {_lsarsResponseStatus = a})

instance NFData ListScheduledAuditsResponse
