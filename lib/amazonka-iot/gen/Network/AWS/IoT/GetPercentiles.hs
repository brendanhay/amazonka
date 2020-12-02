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
-- Module      : Network.AWS.IoT.GetPercentiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Groups the aggregated values that match the query into percentile groupings. The default percentile groupings are: 1,5,25,50,75,95,99, although you can specify your own when you call @GetPercentiles@ . This function returns a value for each percentile group specified (or the default percentile groupings). The percentile group "1" contains the aggregated field value that occurs in approximately one percent of the values that match the query. The percentile group "5" contains the aggregated field value that occurs in approximately five percent of the values that match the query, and so on. The result is an approximation, the more values that match the query, the more accurate the percentile values.
module Network.AWS.IoT.GetPercentiles
  ( -- * Creating a Request
    getPercentiles,
    GetPercentiles,

    -- * Request Lenses
    gpPercents,
    gpQueryVersion,
    gpAggregationField,
    gpIndexName,
    gpQueryString,

    -- * Destructuring the Response
    getPercentilesResponse,
    GetPercentilesResponse,

    -- * Response Lenses
    grsPercentiles,
    grsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPercentiles' smart constructor.
data GetPercentiles = GetPercentiles'
  { _gpPercents ::
      !(Maybe [Double]),
    _gpQueryVersion :: !(Maybe Text),
    _gpAggregationField :: !(Maybe Text),
    _gpIndexName :: !(Maybe Text),
    _gpQueryString :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPercentiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpPercents' - The percentile groups returned.
--
-- * 'gpQueryVersion' - The query version.
--
-- * 'gpAggregationField' - The field to aggregate.
--
-- * 'gpIndexName' - The name of the index to search.
--
-- * 'gpQueryString' - The query string.
getPercentiles ::
  -- | 'gpQueryString'
  Text ->
  GetPercentiles
getPercentiles pQueryString_ =
  GetPercentiles'
    { _gpPercents = Nothing,
      _gpQueryVersion = Nothing,
      _gpAggregationField = Nothing,
      _gpIndexName = Nothing,
      _gpQueryString = pQueryString_
    }

-- | The percentile groups returned.
gpPercents :: Lens' GetPercentiles [Double]
gpPercents = lens _gpPercents (\s a -> s {_gpPercents = a}) . _Default . _Coerce

-- | The query version.
gpQueryVersion :: Lens' GetPercentiles (Maybe Text)
gpQueryVersion = lens _gpQueryVersion (\s a -> s {_gpQueryVersion = a})

-- | The field to aggregate.
gpAggregationField :: Lens' GetPercentiles (Maybe Text)
gpAggregationField = lens _gpAggregationField (\s a -> s {_gpAggregationField = a})

-- | The name of the index to search.
gpIndexName :: Lens' GetPercentiles (Maybe Text)
gpIndexName = lens _gpIndexName (\s a -> s {_gpIndexName = a})

-- | The query string.
gpQueryString :: Lens' GetPercentiles Text
gpQueryString = lens _gpQueryString (\s a -> s {_gpQueryString = a})

instance AWSRequest GetPercentiles where
  type Rs GetPercentiles = GetPercentilesResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          GetPercentilesResponse'
            <$> (x .?> "percentiles" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable GetPercentiles

instance NFData GetPercentiles

instance ToHeaders GetPercentiles where
  toHeaders = const mempty

instance ToJSON GetPercentiles where
  toJSON GetPercentiles' {..} =
    object
      ( catMaybes
          [ ("percents" .=) <$> _gpPercents,
            ("queryVersion" .=) <$> _gpQueryVersion,
            ("aggregationField" .=) <$> _gpAggregationField,
            ("indexName" .=) <$> _gpIndexName,
            Just ("queryString" .= _gpQueryString)
          ]
      )

instance ToPath GetPercentiles where
  toPath = const "/indices/percentiles"

instance ToQuery GetPercentiles where
  toQuery = const mempty

-- | /See:/ 'getPercentilesResponse' smart constructor.
data GetPercentilesResponse = GetPercentilesResponse'
  { _grsPercentiles ::
      !(Maybe [PercentPair]),
    _grsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPercentilesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grsPercentiles' - The percentile values of the aggregated fields.
--
-- * 'grsResponseStatus' - -- | The response status code.
getPercentilesResponse ::
  -- | 'grsResponseStatus'
  Int ->
  GetPercentilesResponse
getPercentilesResponse pResponseStatus_ =
  GetPercentilesResponse'
    { _grsPercentiles = Nothing,
      _grsResponseStatus = pResponseStatus_
    }

-- | The percentile values of the aggregated fields.
grsPercentiles :: Lens' GetPercentilesResponse [PercentPair]
grsPercentiles = lens _grsPercentiles (\s a -> s {_grsPercentiles = a}) . _Default . _Coerce

-- | -- | The response status code.
grsResponseStatus :: Lens' GetPercentilesResponse Int
grsResponseStatus = lens _grsResponseStatus (\s a -> s {_grsResponseStatus = a})

instance NFData GetPercentilesResponse
