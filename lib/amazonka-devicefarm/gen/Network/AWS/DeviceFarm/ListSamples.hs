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
-- Module      : Network.AWS.DeviceFarm.ListSamples
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about samples, given an AWS Device Farm job ARN.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListSamples
  ( -- * Creating a Request
    listSamples,
    ListSamples,

    -- * Request Lenses
    lsNextToken,
    lsArn,

    -- * Destructuring the Response
    listSamplesResponse,
    ListSamplesResponse,

    -- * Response Lenses
    lssrsNextToken,
    lssrsSamples,
    lssrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the list samples operation.
--
--
--
-- /See:/ 'listSamples' smart constructor.
data ListSamples = ListSamples'
  { _lsNextToken :: !(Maybe Text),
    _lsArn :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSamples' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'lsArn' - The Amazon Resource Name (ARN) of the job used to list samples.
listSamples ::
  -- | 'lsArn'
  Text ->
  ListSamples
listSamples pArn_ =
  ListSamples' {_lsNextToken = Nothing, _lsArn = pArn_}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
lsNextToken :: Lens' ListSamples (Maybe Text)
lsNextToken = lens _lsNextToken (\s a -> s {_lsNextToken = a})

-- | The Amazon Resource Name (ARN) of the job used to list samples.
lsArn :: Lens' ListSamples Text
lsArn = lens _lsArn (\s a -> s {_lsArn = a})

instance AWSPager ListSamples where
  page rq rs
    | stop (rs ^. lssrsNextToken) = Nothing
    | stop (rs ^. lssrsSamples) = Nothing
    | otherwise = Just $ rq & lsNextToken .~ rs ^. lssrsNextToken

instance AWSRequest ListSamples where
  type Rs ListSamples = ListSamplesResponse
  request = postJSON deviceFarm
  response =
    receiveJSON
      ( \s h x ->
          ListSamplesResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "samples" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListSamples

instance NFData ListSamples

instance ToHeaders ListSamples where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DeviceFarm_20150623.ListSamples" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListSamples where
  toJSON ListSamples' {..} =
    object
      ( catMaybes
          [("nextToken" .=) <$> _lsNextToken, Just ("arn" .= _lsArn)]
      )

instance ToPath ListSamples where
  toPath = const "/"

instance ToQuery ListSamples where
  toQuery = const mempty

-- | Represents the result of a list samples request.
--
--
--
-- /See:/ 'listSamplesResponse' smart constructor.
data ListSamplesResponse = ListSamplesResponse'
  { _lssrsNextToken ::
      !(Maybe Text),
    _lssrsSamples :: !(Maybe [Sample]),
    _lssrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSamplesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lssrsNextToken' - If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- * 'lssrsSamples' - Information about the samples.
--
-- * 'lssrsResponseStatus' - -- | The response status code.
listSamplesResponse ::
  -- | 'lssrsResponseStatus'
  Int ->
  ListSamplesResponse
listSamplesResponse pResponseStatus_ =
  ListSamplesResponse'
    { _lssrsNextToken = Nothing,
      _lssrsSamples = Nothing,
      _lssrsResponseStatus = pResponseStatus_
    }

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
lssrsNextToken :: Lens' ListSamplesResponse (Maybe Text)
lssrsNextToken = lens _lssrsNextToken (\s a -> s {_lssrsNextToken = a})

-- | Information about the samples.
lssrsSamples :: Lens' ListSamplesResponse [Sample]
lssrsSamples = lens _lssrsSamples (\s a -> s {_lssrsSamples = a}) . _Default . _Coerce

-- | -- | The response status code.
lssrsResponseStatus :: Lens' ListSamplesResponse Int
lssrsResponseStatus = lens _lssrsResponseStatus (\s a -> s {_lssrsResponseStatus = a})

instance NFData ListSamplesResponse
