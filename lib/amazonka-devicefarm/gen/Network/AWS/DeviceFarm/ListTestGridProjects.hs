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
-- Module      : Network.AWS.DeviceFarm.ListTestGridProjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all Selenium testing projects in your account.
module Network.AWS.DeviceFarm.ListTestGridProjects
  ( -- * Creating a Request
    listTestGridProjects,
    ListTestGridProjects,

    -- * Request Lenses
    ltgpMaxResult,
    ltgpNextToken,

    -- * Destructuring the Response
    listTestGridProjectsResponse,
    ListTestGridProjectsResponse,

    -- * Response Lenses
    ltgprsTestGridProjects,
    ltgprsNextToken,
    ltgprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTestGridProjects' smart constructor.
data ListTestGridProjects = ListTestGridProjects'
  { _ltgpMaxResult ::
      !(Maybe Nat),
    _ltgpNextToken :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTestGridProjects' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltgpMaxResult' - Return no more than this number of results.
--
-- * 'ltgpNextToken' - From a response, used to continue a paginated listing.
listTestGridProjects ::
  ListTestGridProjects
listTestGridProjects =
  ListTestGridProjects'
    { _ltgpMaxResult = Nothing,
      _ltgpNextToken = Nothing
    }

-- | Return no more than this number of results.
ltgpMaxResult :: Lens' ListTestGridProjects (Maybe Natural)
ltgpMaxResult = lens _ltgpMaxResult (\s a -> s {_ltgpMaxResult = a}) . mapping _Nat

-- | From a response, used to continue a paginated listing.
ltgpNextToken :: Lens' ListTestGridProjects (Maybe Text)
ltgpNextToken = lens _ltgpNextToken (\s a -> s {_ltgpNextToken = a})

instance AWSRequest ListTestGridProjects where
  type Rs ListTestGridProjects = ListTestGridProjectsResponse
  request = postJSON deviceFarm
  response =
    receiveJSON
      ( \s h x ->
          ListTestGridProjectsResponse'
            <$> (x .?> "testGridProjects" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListTestGridProjects

instance NFData ListTestGridProjects

instance ToHeaders ListTestGridProjects where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DeviceFarm_20150623.ListTestGridProjects" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListTestGridProjects where
  toJSON ListTestGridProjects' {..} =
    object
      ( catMaybes
          [ ("maxResult" .=) <$> _ltgpMaxResult,
            ("nextToken" .=) <$> _ltgpNextToken
          ]
      )

instance ToPath ListTestGridProjects where
  toPath = const "/"

instance ToQuery ListTestGridProjects where
  toQuery = const mempty

-- | /See:/ 'listTestGridProjectsResponse' smart constructor.
data ListTestGridProjectsResponse = ListTestGridProjectsResponse'
  { _ltgprsTestGridProjects ::
      !(Maybe [TestGridProject]),
    _ltgprsNextToken :: !(Maybe Text),
    _ltgprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTestGridProjectsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltgprsTestGridProjects' - The list of TestGridProjects, based on a 'ListTestGridProjectsRequest' .
--
-- * 'ltgprsNextToken' - Used for pagination. Pass into 'ListTestGridProjects' to get more results in a paginated request.
--
-- * 'ltgprsResponseStatus' - -- | The response status code.
listTestGridProjectsResponse ::
  -- | 'ltgprsResponseStatus'
  Int ->
  ListTestGridProjectsResponse
listTestGridProjectsResponse pResponseStatus_ =
  ListTestGridProjectsResponse'
    { _ltgprsTestGridProjects = Nothing,
      _ltgprsNextToken = Nothing,
      _ltgprsResponseStatus = pResponseStatus_
    }

-- | The list of TestGridProjects, based on a 'ListTestGridProjectsRequest' .
ltgprsTestGridProjects :: Lens' ListTestGridProjectsResponse [TestGridProject]
ltgprsTestGridProjects = lens _ltgprsTestGridProjects (\s a -> s {_ltgprsTestGridProjects = a}) . _Default . _Coerce

-- | Used for pagination. Pass into 'ListTestGridProjects' to get more results in a paginated request.
ltgprsNextToken :: Lens' ListTestGridProjectsResponse (Maybe Text)
ltgprsNextToken = lens _ltgprsNextToken (\s a -> s {_ltgprsNextToken = a})

-- | -- | The response status code.
ltgprsResponseStatus :: Lens' ListTestGridProjectsResponse Int
ltgprsResponseStatus = lens _ltgprsResponseStatus (\s a -> s {_ltgprsResponseStatus = a})

instance NFData ListTestGridProjectsResponse
