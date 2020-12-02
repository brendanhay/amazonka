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
-- Module      : Network.AWS.EMR.ListStudios
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all Amazon EMR Studios associated with the AWS account. The list includes details such as ID, Studio Access URL, and creation time for each Studio.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListStudios
  ( -- * Creating a Request
    listStudios,
    ListStudios,

    -- * Request Lenses
    lMarker,

    -- * Destructuring the Response
    listStudiosResponse,
    ListStudiosResponse,

    -- * Response Lenses
    lsrsStudios,
    lsrsMarker,
    lsrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listStudios' smart constructor.
newtype ListStudios = ListStudios' {_lMarker :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListStudios' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lMarker' - The pagination token that indicates the set of results to retrieve.
listStudios ::
  ListStudios
listStudios = ListStudios' {_lMarker = Nothing}

-- | The pagination token that indicates the set of results to retrieve.
lMarker :: Lens' ListStudios (Maybe Text)
lMarker = lens _lMarker (\s a -> s {_lMarker = a})

instance AWSPager ListStudios where
  page rq rs
    | stop (rs ^. lsrsMarker) = Nothing
    | stop (rs ^. lsrsStudios) = Nothing
    | otherwise = Just $ rq & lMarker .~ rs ^. lsrsMarker

instance AWSRequest ListStudios where
  type Rs ListStudios = ListStudiosResponse
  request = postJSON emr
  response =
    receiveJSON
      ( \s h x ->
          ListStudiosResponse'
            <$> (x .?> "Studios" .!@ mempty)
            <*> (x .?> "Marker")
            <*> (pure (fromEnum s))
      )

instance Hashable ListStudios

instance NFData ListStudios

instance ToHeaders ListStudios where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("ElasticMapReduce.ListStudios" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListStudios where
  toJSON ListStudios' {..} =
    object (catMaybes [("Marker" .=) <$> _lMarker])

instance ToPath ListStudios where
  toPath = const "/"

instance ToQuery ListStudios where
  toQuery = const mempty

-- | /See:/ 'listStudiosResponse' smart constructor.
data ListStudiosResponse = ListStudiosResponse'
  { _lsrsStudios ::
      !(Maybe [StudioSummary]),
    _lsrsMarker :: !(Maybe Text),
    _lsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListStudiosResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrsStudios' - The list of Studio summary objects.
--
-- * 'lsrsMarker' - The pagination token that indicates the next set of results to retrieve.
--
-- * 'lsrsResponseStatus' - -- | The response status code.
listStudiosResponse ::
  -- | 'lsrsResponseStatus'
  Int ->
  ListStudiosResponse
listStudiosResponse pResponseStatus_ =
  ListStudiosResponse'
    { _lsrsStudios = Nothing,
      _lsrsMarker = Nothing,
      _lsrsResponseStatus = pResponseStatus_
    }

-- | The list of Studio summary objects.
lsrsStudios :: Lens' ListStudiosResponse [StudioSummary]
lsrsStudios = lens _lsrsStudios (\s a -> s {_lsrsStudios = a}) . _Default . _Coerce

-- | The pagination token that indicates the next set of results to retrieve.
lsrsMarker :: Lens' ListStudiosResponse (Maybe Text)
lsrsMarker = lens _lsrsMarker (\s a -> s {_lsrsMarker = a})

-- | -- | The response status code.
lsrsResponseStatus :: Lens' ListStudiosResponse Int
lsrsResponseStatus = lens _lsrsResponseStatus (\s a -> s {_lsrsResponseStatus = a})

instance NFData ListStudiosResponse
