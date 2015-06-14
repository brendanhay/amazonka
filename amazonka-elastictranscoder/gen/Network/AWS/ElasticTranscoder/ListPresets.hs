{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticTranscoder.ListPresets
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The ListPresets operation gets a list of the default presets included
-- with Elastic Transcoder and the presets that you\'ve added in an AWS
-- region.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/ListPresets.html>
module Network.AWS.ElasticTranscoder.ListPresets
    (
    -- * Request
      ListPresets
    -- ** Request constructor
    , listPresets
    -- ** Request lenses
    , lisAscending
    , lisPageToken

    -- * Response
    , ListPresetsResponse
    -- ** Response constructor
    , listPresetsResponse
    -- ** Response lenses
    , lisNextPageToken
    , lisPresets
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticTranscoder.Types

-- | /See:/ 'listPresets' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lisAscending'
--
-- * 'lisPageToken'
data ListPresets = ListPresets'{_lisAscending :: Maybe Text, _lisPageToken :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListPresets' smart constructor.
listPresets :: ListPresets
listPresets = ListPresets'{_lisAscending = Nothing, _lisPageToken = Nothing};

-- | To list presets in chronological order by the date and time that they
-- were created, enter @true@. To list presets in reverse chronological
-- order, enter @false@.
lisAscending :: Lens' ListPresets (Maybe Text)
lisAscending = lens _lisAscending (\ s a -> s{_lisAscending = a});

-- | When Elastic Transcoder returns more than one page of results, use
-- @pageToken@ in subsequent @GET@ requests to get each successive page of
-- results.
lisPageToken :: Lens' ListPresets (Maybe Text)
lisPageToken = lens _lisPageToken (\ s a -> s{_lisPageToken = a});

instance AWSRequest ListPresets where
        type Sv ListPresets = ElasticTranscoder
        type Rs ListPresets = ListPresetsResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 ListPresetsResponse' <$>
                   x .?> "NextPageToken" <*> x .?> "Presets" .!@ mempty)

instance ToHeaders ListPresets where
        toHeaders = const mempty

instance ToPath ListPresets where
        toPath = const "/2012-09-25/presets"

instance ToQuery ListPresets where
        toQuery ListPresets'{..}
          = mconcat
              ["Ascending" =: _lisAscending,
               "PageToken" =: _lisPageToken]

-- | /See:/ 'listPresetsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lisNextPageToken'
--
-- * 'lisPresets'
data ListPresetsResponse = ListPresetsResponse'{_lisNextPageToken :: Maybe Text, _lisPresets :: [Preset]} deriving (Eq, Read, Show)

-- | 'ListPresetsResponse' smart constructor.
listPresetsResponse :: ListPresetsResponse
listPresetsResponse = ListPresetsResponse'{_lisNextPageToken = Nothing, _lisPresets = mempty};

-- | A value that you use to access the second and subsequent pages of
-- results, if any. When the presets fit on one page or when you\'ve
-- reached the last page of results, the value of @NextPageToken@ is
-- @null@.
lisNextPageToken :: Lens' ListPresetsResponse (Maybe Text)
lisNextPageToken = lens _lisNextPageToken (\ s a -> s{_lisNextPageToken = a});

-- | An array of @Preset@ objects.
lisPresets :: Lens' ListPresetsResponse [Preset]
lisPresets = lens _lisPresets (\ s a -> s{_lisPresets = a});
