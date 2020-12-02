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
-- Module      : Network.AWS.Lambda.ListCodeSigningConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of <https://docs.aws.amazon.com/lambda/latest/dg/configuring-codesigning.html code signing configurations> for the specified function. A request returns up to 10,000 configurations per call. You can use the @MaxItems@ parameter to return fewer configurations per call.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListCodeSigningConfigs
  ( -- * Creating a Request
    listCodeSigningConfigs,
    ListCodeSigningConfigs,

    -- * Request Lenses
    lcscMarker,
    lcscMaxItems,

    -- * Destructuring the Response
    listCodeSigningConfigsResponse,
    ListCodeSigningConfigsResponse,

    -- * Response Lenses
    lcscrsCodeSigningConfigs,
    lcscrsNextMarker,
    lcscrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listCodeSigningConfigs' smart constructor.
data ListCodeSigningConfigs = ListCodeSigningConfigs'
  { _lcscMarker ::
      !(Maybe Text),
    _lcscMaxItems :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListCodeSigningConfigs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcscMarker' - Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- * 'lcscMaxItems' - Maximum number of items to return.
listCodeSigningConfigs ::
  ListCodeSigningConfigs
listCodeSigningConfigs =
  ListCodeSigningConfigs'
    { _lcscMarker = Nothing,
      _lcscMaxItems = Nothing
    }

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
lcscMarker :: Lens' ListCodeSigningConfigs (Maybe Text)
lcscMarker = lens _lcscMarker (\s a -> s {_lcscMarker = a})

-- | Maximum number of items to return.
lcscMaxItems :: Lens' ListCodeSigningConfigs (Maybe Natural)
lcscMaxItems = lens _lcscMaxItems (\s a -> s {_lcscMaxItems = a}) . mapping _Nat

instance AWSPager ListCodeSigningConfigs where
  page rq rs
    | stop (rs ^. lcscrsNextMarker) = Nothing
    | stop (rs ^. lcscrsCodeSigningConfigs) = Nothing
    | otherwise = Just $ rq & lcscMarker .~ rs ^. lcscrsNextMarker

instance AWSRequest ListCodeSigningConfigs where
  type Rs ListCodeSigningConfigs = ListCodeSigningConfigsResponse
  request = get lambda
  response =
    receiveJSON
      ( \s h x ->
          ListCodeSigningConfigsResponse'
            <$> (x .?> "CodeSigningConfigs" .!@ mempty)
            <*> (x .?> "NextMarker")
            <*> (pure (fromEnum s))
      )

instance Hashable ListCodeSigningConfigs

instance NFData ListCodeSigningConfigs

instance ToHeaders ListCodeSigningConfigs where
  toHeaders = const mempty

instance ToPath ListCodeSigningConfigs where
  toPath = const "/2020-04-22/code-signing-configs/"

instance ToQuery ListCodeSigningConfigs where
  toQuery ListCodeSigningConfigs' {..} =
    mconcat ["Marker" =: _lcscMarker, "MaxItems" =: _lcscMaxItems]

-- | /See:/ 'listCodeSigningConfigsResponse' smart constructor.
data ListCodeSigningConfigsResponse = ListCodeSigningConfigsResponse'
  { _lcscrsCodeSigningConfigs ::
      !(Maybe [CodeSigningConfig]),
    _lcscrsNextMarker ::
      !(Maybe Text),
    _lcscrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListCodeSigningConfigsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcscrsCodeSigningConfigs' - The code signing configurations
--
-- * 'lcscrsNextMarker' - The pagination token that's included if more results are available.
--
-- * 'lcscrsResponseStatus' - -- | The response status code.
listCodeSigningConfigsResponse ::
  -- | 'lcscrsResponseStatus'
  Int ->
  ListCodeSigningConfigsResponse
listCodeSigningConfigsResponse pResponseStatus_ =
  ListCodeSigningConfigsResponse'
    { _lcscrsCodeSigningConfigs =
        Nothing,
      _lcscrsNextMarker = Nothing,
      _lcscrsResponseStatus = pResponseStatus_
    }

-- | The code signing configurations
lcscrsCodeSigningConfigs :: Lens' ListCodeSigningConfigsResponse [CodeSigningConfig]
lcscrsCodeSigningConfigs = lens _lcscrsCodeSigningConfigs (\s a -> s {_lcscrsCodeSigningConfigs = a}) . _Default . _Coerce

-- | The pagination token that's included if more results are available.
lcscrsNextMarker :: Lens' ListCodeSigningConfigsResponse (Maybe Text)
lcscrsNextMarker = lens _lcscrsNextMarker (\s a -> s {_lcscrsNextMarker = a})

-- | -- | The response status code.
lcscrsResponseStatus :: Lens' ListCodeSigningConfigsResponse Int
lcscrsResponseStatus = lens _lcscrsResponseStatus (\s a -> s {_lcscrsResponseStatus = a})

instance NFData ListCodeSigningConfigsResponse
