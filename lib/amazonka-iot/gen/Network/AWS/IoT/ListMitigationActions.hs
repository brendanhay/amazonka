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
-- Module      : Network.AWS.IoT.ListMitigationActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all mitigation actions that match the specified filter criteria.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListMitigationActions
  ( -- * Creating a Request
    listMitigationActions,
    ListMitigationActions,

    -- * Request Lenses
    lmaNextToken,
    lmaActionType,
    lmaMaxResults,

    -- * Destructuring the Response
    listMitigationActionsResponse,
    ListMitigationActionsResponse,

    -- * Response Lenses
    lmarsActionIdentifiers,
    lmarsNextToken,
    lmarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listMitigationActions' smart constructor.
data ListMitigationActions = ListMitigationActions'
  { _lmaNextToken ::
      !(Maybe Text),
    _lmaActionType :: !(Maybe MitigationActionType),
    _lmaMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMitigationActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmaNextToken' - The token for the next set of results.
--
-- * 'lmaActionType' - Specify a value to limit the result to mitigation actions with a specific action type.
--
-- * 'lmaMaxResults' - The maximum number of results to return at one time. The default is 25.
listMitigationActions ::
  ListMitigationActions
listMitigationActions =
  ListMitigationActions'
    { _lmaNextToken = Nothing,
      _lmaActionType = Nothing,
      _lmaMaxResults = Nothing
    }

-- | The token for the next set of results.
lmaNextToken :: Lens' ListMitigationActions (Maybe Text)
lmaNextToken = lens _lmaNextToken (\s a -> s {_lmaNextToken = a})

-- | Specify a value to limit the result to mitigation actions with a specific action type.
lmaActionType :: Lens' ListMitigationActions (Maybe MitigationActionType)
lmaActionType = lens _lmaActionType (\s a -> s {_lmaActionType = a})

-- | The maximum number of results to return at one time. The default is 25.
lmaMaxResults :: Lens' ListMitigationActions (Maybe Natural)
lmaMaxResults = lens _lmaMaxResults (\s a -> s {_lmaMaxResults = a}) . mapping _Nat

instance AWSPager ListMitigationActions where
  page rq rs
    | stop (rs ^. lmarsNextToken) = Nothing
    | stop (rs ^. lmarsActionIdentifiers) = Nothing
    | otherwise = Just $ rq & lmaNextToken .~ rs ^. lmarsNextToken

instance AWSRequest ListMitigationActions where
  type Rs ListMitigationActions = ListMitigationActionsResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          ListMitigationActionsResponse'
            <$> (x .?> "actionIdentifiers" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListMitigationActions

instance NFData ListMitigationActions

instance ToHeaders ListMitigationActions where
  toHeaders = const mempty

instance ToPath ListMitigationActions where
  toPath = const "/mitigationactions/actions"

instance ToQuery ListMitigationActions where
  toQuery ListMitigationActions' {..} =
    mconcat
      [ "nextToken" =: _lmaNextToken,
        "actionType" =: _lmaActionType,
        "maxResults" =: _lmaMaxResults
      ]

-- | /See:/ 'listMitigationActionsResponse' smart constructor.
data ListMitigationActionsResponse = ListMitigationActionsResponse'
  { _lmarsActionIdentifiers ::
      !( Maybe
           [MitigationActionIdentifier]
       ),
    _lmarsNextToken ::
      !(Maybe Text),
    _lmarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMitigationActionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmarsActionIdentifiers' - A set of actions that matched the specified filter criteria.
--
-- * 'lmarsNextToken' - The token for the next set of results.
--
-- * 'lmarsResponseStatus' - -- | The response status code.
listMitigationActionsResponse ::
  -- | 'lmarsResponseStatus'
  Int ->
  ListMitigationActionsResponse
listMitigationActionsResponse pResponseStatus_ =
  ListMitigationActionsResponse'
    { _lmarsActionIdentifiers = Nothing,
      _lmarsNextToken = Nothing,
      _lmarsResponseStatus = pResponseStatus_
    }

-- | A set of actions that matched the specified filter criteria.
lmarsActionIdentifiers :: Lens' ListMitigationActionsResponse [MitigationActionIdentifier]
lmarsActionIdentifiers = lens _lmarsActionIdentifiers (\s a -> s {_lmarsActionIdentifiers = a}) . _Default . _Coerce

-- | The token for the next set of results.
lmarsNextToken :: Lens' ListMitigationActionsResponse (Maybe Text)
lmarsNextToken = lens _lmarsNextToken (\s a -> s {_lmarsNextToken = a})

-- | -- | The response status code.
lmarsResponseStatus :: Lens' ListMitigationActionsResponse Int
lmarsResponseStatus = lens _lmarsResponseStatus (\s a -> s {_lmarsResponseStatus = a})

instance NFData ListMitigationActionsResponse
