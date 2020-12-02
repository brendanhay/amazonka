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
-- Module      : Network.AWS.SageMaker.ListSubscribedWorkteams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the work teams that you are subscribed to in the AWS Marketplace. The list may be empty if no work team satisfies the filter specified in the @NameContains@ parameter.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListSubscribedWorkteams
  ( -- * Creating a Request
    listSubscribedWorkteams,
    ListSubscribedWorkteams,

    -- * Request Lenses
    lswNameContains,
    lswNextToken,
    lswMaxResults,

    -- * Destructuring the Response
    listSubscribedWorkteamsResponse,
    ListSubscribedWorkteamsResponse,

    -- * Response Lenses
    lswrsNextToken,
    lswrsResponseStatus,
    lswrsSubscribedWorkteams,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listSubscribedWorkteams' smart constructor.
data ListSubscribedWorkteams = ListSubscribedWorkteams'
  { _lswNameContains ::
      !(Maybe Text),
    _lswNextToken :: !(Maybe Text),
    _lswMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSubscribedWorkteams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lswNameContains' - A string in the work team name. This filter returns only work teams whose name contains the specified string.
--
-- * 'lswNextToken' - If the result of the previous @ListSubscribedWorkteams@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
--
-- * 'lswMaxResults' - The maximum number of work teams to return in each page of the response.
listSubscribedWorkteams ::
  ListSubscribedWorkteams
listSubscribedWorkteams =
  ListSubscribedWorkteams'
    { _lswNameContains = Nothing,
      _lswNextToken = Nothing,
      _lswMaxResults = Nothing
    }

-- | A string in the work team name. This filter returns only work teams whose name contains the specified string.
lswNameContains :: Lens' ListSubscribedWorkteams (Maybe Text)
lswNameContains = lens _lswNameContains (\s a -> s {_lswNameContains = a})

-- | If the result of the previous @ListSubscribedWorkteams@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
lswNextToken :: Lens' ListSubscribedWorkteams (Maybe Text)
lswNextToken = lens _lswNextToken (\s a -> s {_lswNextToken = a})

-- | The maximum number of work teams to return in each page of the response.
lswMaxResults :: Lens' ListSubscribedWorkteams (Maybe Natural)
lswMaxResults = lens _lswMaxResults (\s a -> s {_lswMaxResults = a}) . mapping _Nat

instance AWSPager ListSubscribedWorkteams where
  page rq rs
    | stop (rs ^. lswrsNextToken) = Nothing
    | stop (rs ^. lswrsSubscribedWorkteams) = Nothing
    | otherwise = Just $ rq & lswNextToken .~ rs ^. lswrsNextToken

instance AWSRequest ListSubscribedWorkteams where
  type Rs ListSubscribedWorkteams = ListSubscribedWorkteamsResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListSubscribedWorkteamsResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "SubscribedWorkteams" .!@ mempty)
      )

instance Hashable ListSubscribedWorkteams

instance NFData ListSubscribedWorkteams

instance ToHeaders ListSubscribedWorkteams where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.ListSubscribedWorkteams" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListSubscribedWorkteams where
  toJSON ListSubscribedWorkteams' {..} =
    object
      ( catMaybes
          [ ("NameContains" .=) <$> _lswNameContains,
            ("NextToken" .=) <$> _lswNextToken,
            ("MaxResults" .=) <$> _lswMaxResults
          ]
      )

instance ToPath ListSubscribedWorkteams where
  toPath = const "/"

instance ToQuery ListSubscribedWorkteams where
  toQuery = const mempty

-- | /See:/ 'listSubscribedWorkteamsResponse' smart constructor.
data ListSubscribedWorkteamsResponse = ListSubscribedWorkteamsResponse'
  { _lswrsNextToken ::
      !(Maybe Text),
    _lswrsResponseStatus ::
      !Int,
    _lswrsSubscribedWorkteams ::
      ![SubscribedWorkteam]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSubscribedWorkteamsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lswrsNextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of work teams, use it in the subsequent request.
--
-- * 'lswrsResponseStatus' - -- | The response status code.
--
-- * 'lswrsSubscribedWorkteams' - An array of @Workteam@ objects, each describing a work team.
listSubscribedWorkteamsResponse ::
  -- | 'lswrsResponseStatus'
  Int ->
  ListSubscribedWorkteamsResponse
listSubscribedWorkteamsResponse pResponseStatus_ =
  ListSubscribedWorkteamsResponse'
    { _lswrsNextToken = Nothing,
      _lswrsResponseStatus = pResponseStatus_,
      _lswrsSubscribedWorkteams = mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of work teams, use it in the subsequent request.
lswrsNextToken :: Lens' ListSubscribedWorkteamsResponse (Maybe Text)
lswrsNextToken = lens _lswrsNextToken (\s a -> s {_lswrsNextToken = a})

-- | -- | The response status code.
lswrsResponseStatus :: Lens' ListSubscribedWorkteamsResponse Int
lswrsResponseStatus = lens _lswrsResponseStatus (\s a -> s {_lswrsResponseStatus = a})

-- | An array of @Workteam@ objects, each describing a work team.
lswrsSubscribedWorkteams :: Lens' ListSubscribedWorkteamsResponse [SubscribedWorkteam]
lswrsSubscribedWorkteams = lens _lswrsSubscribedWorkteams (\s a -> s {_lswrsSubscribedWorkteams = a}) . _Coerce

instance NFData ListSubscribedWorkteamsResponse
