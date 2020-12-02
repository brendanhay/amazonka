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
-- Module      : Network.AWS.DeviceFarm.ListTestGridSessionActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the actions taken in a 'TestGridSession' .
module Network.AWS.DeviceFarm.ListTestGridSessionActions
  ( -- * Creating a Request
    listTestGridSessionActions,
    ListTestGridSessionActions,

    -- * Request Lenses
    ltgsaMaxResult,
    ltgsaNextToken,
    ltgsaSessionARN,

    -- * Destructuring the Response
    listTestGridSessionActionsResponse,
    ListTestGridSessionActionsResponse,

    -- * Response Lenses
    ltgsarsActions,
    ltgsarsNextToken,
    ltgsarsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTestGridSessionActions' smart constructor.
data ListTestGridSessionActions = ListTestGridSessionActions'
  { _ltgsaMaxResult ::
      !(Maybe Nat),
    _ltgsaNextToken :: !(Maybe Text),
    _ltgsaSessionARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTestGridSessionActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltgsaMaxResult' - The maximum number of sessions to return per response.
--
-- * 'ltgsaNextToken' - Pagination token.
--
-- * 'ltgsaSessionARN' - The ARN of the session to retrieve.
listTestGridSessionActions ::
  -- | 'ltgsaSessionARN'
  Text ->
  ListTestGridSessionActions
listTestGridSessionActions pSessionARN_ =
  ListTestGridSessionActions'
    { _ltgsaMaxResult = Nothing,
      _ltgsaNextToken = Nothing,
      _ltgsaSessionARN = pSessionARN_
    }

-- | The maximum number of sessions to return per response.
ltgsaMaxResult :: Lens' ListTestGridSessionActions (Maybe Natural)
ltgsaMaxResult = lens _ltgsaMaxResult (\s a -> s {_ltgsaMaxResult = a}) . mapping _Nat

-- | Pagination token.
ltgsaNextToken :: Lens' ListTestGridSessionActions (Maybe Text)
ltgsaNextToken = lens _ltgsaNextToken (\s a -> s {_ltgsaNextToken = a})

-- | The ARN of the session to retrieve.
ltgsaSessionARN :: Lens' ListTestGridSessionActions Text
ltgsaSessionARN = lens _ltgsaSessionARN (\s a -> s {_ltgsaSessionARN = a})

instance AWSRequest ListTestGridSessionActions where
  type
    Rs ListTestGridSessionActions =
      ListTestGridSessionActionsResponse
  request = postJSON deviceFarm
  response =
    receiveJSON
      ( \s h x ->
          ListTestGridSessionActionsResponse'
            <$> (x .?> "actions" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListTestGridSessionActions

instance NFData ListTestGridSessionActions

instance ToHeaders ListTestGridSessionActions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DeviceFarm_20150623.ListTestGridSessionActions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListTestGridSessionActions where
  toJSON ListTestGridSessionActions' {..} =
    object
      ( catMaybes
          [ ("maxResult" .=) <$> _ltgsaMaxResult,
            ("nextToken" .=) <$> _ltgsaNextToken,
            Just ("sessionArn" .= _ltgsaSessionARN)
          ]
      )

instance ToPath ListTestGridSessionActions where
  toPath = const "/"

instance ToQuery ListTestGridSessionActions where
  toQuery = const mempty

-- | /See:/ 'listTestGridSessionActionsResponse' smart constructor.
data ListTestGridSessionActionsResponse = ListTestGridSessionActionsResponse'
  { _ltgsarsActions ::
      !( Maybe
           [TestGridSessionAction]
       ),
    _ltgsarsNextToken ::
      !(Maybe Text),
    _ltgsarsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTestGridSessionActionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltgsarsActions' - The action taken by the session.
--
-- * 'ltgsarsNextToken' - Pagination token.
--
-- * 'ltgsarsResponseStatus' - -- | The response status code.
listTestGridSessionActionsResponse ::
  -- | 'ltgsarsResponseStatus'
  Int ->
  ListTestGridSessionActionsResponse
listTestGridSessionActionsResponse pResponseStatus_ =
  ListTestGridSessionActionsResponse'
    { _ltgsarsActions = Nothing,
      _ltgsarsNextToken = Nothing,
      _ltgsarsResponseStatus = pResponseStatus_
    }

-- | The action taken by the session.
ltgsarsActions :: Lens' ListTestGridSessionActionsResponse [TestGridSessionAction]
ltgsarsActions = lens _ltgsarsActions (\s a -> s {_ltgsarsActions = a}) . _Default . _Coerce

-- | Pagination token.
ltgsarsNextToken :: Lens' ListTestGridSessionActionsResponse (Maybe Text)
ltgsarsNextToken = lens _ltgsarsNextToken (\s a -> s {_ltgsarsNextToken = a})

-- | -- | The response status code.
ltgsarsResponseStatus :: Lens' ListTestGridSessionActionsResponse Int
ltgsarsResponseStatus = lens _ltgsarsResponseStatus (\s a -> s {_ltgsarsResponseStatus = a})

instance NFData ListTestGridSessionActionsResponse
