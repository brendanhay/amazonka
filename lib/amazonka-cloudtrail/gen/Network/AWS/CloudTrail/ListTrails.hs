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
-- Module      : Network.AWS.CloudTrail.ListTrails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists trails that are in the current account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudTrail.ListTrails
  ( -- * Creating a Request
    listTrails,
    ListTrails,

    -- * Request Lenses
    lNextToken,

    -- * Destructuring the Response
    listTrailsResponse,
    ListTrailsResponse,

    -- * Response Lenses
    lrsNextToken,
    lrsTrails,
    lrsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTrails' smart constructor.
newtype ListTrails = ListTrails' {_lNextToken :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTrails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lNextToken' - The token to use to get the next page of results after a previous API call. This token must be passed in with the same parameters that were specified in the the original call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
listTrails ::
  ListTrails
listTrails = ListTrails' {_lNextToken = Nothing}

-- | The token to use to get the next page of results after a previous API call. This token must be passed in with the same parameters that were specified in the the original call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
lNextToken :: Lens' ListTrails (Maybe Text)
lNextToken = lens _lNextToken (\s a -> s {_lNextToken = a})

instance AWSPager ListTrails where
  page rq rs
    | stop (rs ^. lrsNextToken) = Nothing
    | stop (rs ^. lrsTrails) = Nothing
    | otherwise = Just $ rq & lNextToken .~ rs ^. lrsNextToken

instance AWSRequest ListTrails where
  type Rs ListTrails = ListTrailsResponse
  request = postJSON cloudTrail
  response =
    receiveJSON
      ( \s h x ->
          ListTrailsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "Trails" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListTrails

instance NFData ListTrails

instance ToHeaders ListTrails where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListTrails" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListTrails where
  toJSON ListTrails' {..} =
    object (catMaybes [("NextToken" .=) <$> _lNextToken])

instance ToPath ListTrails where
  toPath = const "/"

instance ToQuery ListTrails where
  toQuery = const mempty

-- | /See:/ 'listTrailsResponse' smart constructor.
data ListTrailsResponse = ListTrailsResponse'
  { _lrsNextToken ::
      !(Maybe Text),
    _lrsTrails :: !(Maybe [TrailInfo]),
    _lrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTrailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsNextToken' - The token to use to get the next page of results after a previous API call. If the token does not appear, there are no more results to return. The token must be passed in with the same parameters as the previous call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
--
-- * 'lrsTrails' - Returns the name, ARN, and home region of trails in the current account.
--
-- * 'lrsResponseStatus' - -- | The response status code.
listTrailsResponse ::
  -- | 'lrsResponseStatus'
  Int ->
  ListTrailsResponse
listTrailsResponse pResponseStatus_ =
  ListTrailsResponse'
    { _lrsNextToken = Nothing,
      _lrsTrails = Nothing,
      _lrsResponseStatus = pResponseStatus_
    }

-- | The token to use to get the next page of results after a previous API call. If the token does not appear, there are no more results to return. The token must be passed in with the same parameters as the previous call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
lrsNextToken :: Lens' ListTrailsResponse (Maybe Text)
lrsNextToken = lens _lrsNextToken (\s a -> s {_lrsNextToken = a})

-- | Returns the name, ARN, and home region of trails in the current account.
lrsTrails :: Lens' ListTrailsResponse [TrailInfo]
lrsTrails = lens _lrsTrails (\s a -> s {_lrsTrails = a}) . _Default . _Coerce

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListTrailsResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\s a -> s {_lrsResponseStatus = a})

instance NFData ListTrailsResponse
