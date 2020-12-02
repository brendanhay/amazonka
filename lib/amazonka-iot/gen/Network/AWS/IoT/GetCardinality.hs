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
-- Module      : Network.AWS.IoT.GetCardinality
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the approximate count of unique values that match the query.
module Network.AWS.IoT.GetCardinality
  ( -- * Creating a Request
    getCardinality,
    GetCardinality,

    -- * Request Lenses
    gcQueryVersion,
    gcAggregationField,
    gcIndexName,
    gcQueryString,

    -- * Destructuring the Response
    getCardinalityResponse,
    GetCardinalityResponse,

    -- * Response Lenses
    gcrsCardinality,
    gcrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCardinality' smart constructor.
data GetCardinality = GetCardinality'
  { _gcQueryVersion ::
      !(Maybe Text),
    _gcAggregationField :: !(Maybe Text),
    _gcIndexName :: !(Maybe Text),
    _gcQueryString :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCardinality' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcQueryVersion' - The query version.
--
-- * 'gcAggregationField' - The field to aggregate.
--
-- * 'gcIndexName' - The name of the index to search.
--
-- * 'gcQueryString' - The search query.
getCardinality ::
  -- | 'gcQueryString'
  Text ->
  GetCardinality
getCardinality pQueryString_ =
  GetCardinality'
    { _gcQueryVersion = Nothing,
      _gcAggregationField = Nothing,
      _gcIndexName = Nothing,
      _gcQueryString = pQueryString_
    }

-- | The query version.
gcQueryVersion :: Lens' GetCardinality (Maybe Text)
gcQueryVersion = lens _gcQueryVersion (\s a -> s {_gcQueryVersion = a})

-- | The field to aggregate.
gcAggregationField :: Lens' GetCardinality (Maybe Text)
gcAggregationField = lens _gcAggregationField (\s a -> s {_gcAggregationField = a})

-- | The name of the index to search.
gcIndexName :: Lens' GetCardinality (Maybe Text)
gcIndexName = lens _gcIndexName (\s a -> s {_gcIndexName = a})

-- | The search query.
gcQueryString :: Lens' GetCardinality Text
gcQueryString = lens _gcQueryString (\s a -> s {_gcQueryString = a})

instance AWSRequest GetCardinality where
  type Rs GetCardinality = GetCardinalityResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          GetCardinalityResponse'
            <$> (x .?> "cardinality") <*> (pure (fromEnum s))
      )

instance Hashable GetCardinality

instance NFData GetCardinality

instance ToHeaders GetCardinality where
  toHeaders = const mempty

instance ToJSON GetCardinality where
  toJSON GetCardinality' {..} =
    object
      ( catMaybes
          [ ("queryVersion" .=) <$> _gcQueryVersion,
            ("aggregationField" .=) <$> _gcAggregationField,
            ("indexName" .=) <$> _gcIndexName,
            Just ("queryString" .= _gcQueryString)
          ]
      )

instance ToPath GetCardinality where
  toPath = const "/indices/cardinality"

instance ToQuery GetCardinality where
  toQuery = const mempty

-- | /See:/ 'getCardinalityResponse' smart constructor.
data GetCardinalityResponse = GetCardinalityResponse'
  { _gcrsCardinality ::
      !(Maybe Int),
    _gcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCardinalityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrsCardinality' - The approximate count of unique values that match the query.
--
-- * 'gcrsResponseStatus' - -- | The response status code.
getCardinalityResponse ::
  -- | 'gcrsResponseStatus'
  Int ->
  GetCardinalityResponse
getCardinalityResponse pResponseStatus_ =
  GetCardinalityResponse'
    { _gcrsCardinality = Nothing,
      _gcrsResponseStatus = pResponseStatus_
    }

-- | The approximate count of unique values that match the query.
gcrsCardinality :: Lens' GetCardinalityResponse (Maybe Int)
gcrsCardinality = lens _gcrsCardinality (\s a -> s {_gcrsCardinality = a})

-- | -- | The response status code.
gcrsResponseStatus :: Lens' GetCardinalityResponse Int
gcrsResponseStatus = lens _gcrsResponseStatus (\s a -> s {_gcrsResponseStatus = a})

instance NFData GetCardinalityResponse
