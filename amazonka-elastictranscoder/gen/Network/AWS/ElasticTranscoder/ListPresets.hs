{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.ListPresets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ListPresets operation gets a list of the default presets included with Elastic Transcoder and the presets that you've added in an AWS region.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ElasticTranscoder.ListPresets
    (
    -- * Creating a Request
      listPresets
    , ListPresets
    -- * Request Lenses
    , lAscending
    , lPageToken

    -- * Destructuring the Response
    , listPresetsResponse
    , ListPresetsResponse
    -- * Response Lenses
    , lrsNextPageToken
    , lrsPresets
    , lrsResponseStatus
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.ElasticTranscoder.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The @ListPresetsRequest@ structure.
--
--
--
-- /See:/ 'listPresets' smart constructor.
data ListPresets = ListPresets'
  { _lAscending :: !(Maybe Text)
  , _lPageToken :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPresets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lAscending' - To list presets in chronological order by the date and time that they were created, enter @true@ . To list presets in reverse chronological order, enter @false@ .
--
-- * 'lPageToken' - When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
listPresets
    :: ListPresets
listPresets = ListPresets' {_lAscending = Nothing, _lPageToken = Nothing}


-- | To list presets in chronological order by the date and time that they were created, enter @true@ . To list presets in reverse chronological order, enter @false@ .
lAscending :: Lens' ListPresets (Maybe Text)
lAscending = lens _lAscending (\ s a -> s{_lAscending = a})

-- | When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
lPageToken :: Lens' ListPresets (Maybe Text)
lPageToken = lens _lPageToken (\ s a -> s{_lPageToken = a})

instance AWSPager ListPresets where
        page rq rs
          | stop (rs ^. lrsNextPageToken) = Nothing
          | stop (rs ^. lrsPresets) = Nothing
          | otherwise =
            Just $ rq & lPageToken .~ rs ^. lrsNextPageToken

instance AWSRequest ListPresets where
        type Rs ListPresets = ListPresetsResponse
        request = get elasticTranscoder
        response
          = receiveJSON
              (\ s h x ->
                 ListPresetsResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "Presets" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListPresets where

instance NFData ListPresets where

instance ToHeaders ListPresets where
        toHeaders = const mempty

instance ToPath ListPresets where
        toPath = const "/2012-09-25/presets"

instance ToQuery ListPresets where
        toQuery ListPresets'{..}
          = mconcat
              ["Ascending" =: _lAscending,
               "PageToken" =: _lPageToken]

-- | The @ListPresetsResponse@ structure.
--
--
--
-- /See:/ 'listPresetsResponse' smart constructor.
data ListPresetsResponse = ListPresetsResponse'
  { _lrsNextPageToken  :: !(Maybe Text)
  , _lrsPresets        :: !(Maybe [Preset])
  , _lrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPresetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsNextPageToken' - A value that you use to access the second and subsequent pages of results, if any. When the presets fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
--
-- * 'lrsPresets' - An array of @Preset@ objects.
--
-- * 'lrsResponseStatus' - -- | The response status code.
listPresetsResponse
    :: Int -- ^ 'lrsResponseStatus'
    -> ListPresetsResponse
listPresetsResponse pResponseStatus_ =
  ListPresetsResponse'
    { _lrsNextPageToken = Nothing
    , _lrsPresets = Nothing
    , _lrsResponseStatus = pResponseStatus_
    }


-- | A value that you use to access the second and subsequent pages of results, if any. When the presets fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
lrsNextPageToken :: Lens' ListPresetsResponse (Maybe Text)
lrsNextPageToken = lens _lrsNextPageToken (\ s a -> s{_lrsNextPageToken = a})

-- | An array of @Preset@ objects.
lrsPresets :: Lens' ListPresetsResponse [Preset]
lrsPresets = lens _lrsPresets (\ s a -> s{_lrsPresets = a}) . _Default . _Coerce

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListPresetsResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\ s a -> s{_lrsResponseStatus = a})

instance NFData ListPresetsResponse where
