{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticTranscoder.ListPipelines
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

-- | The ListPipelines operation gets a list of the pipelines associated with
-- the current AWS account.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/ListPipelines.html>
module Network.AWS.ElasticTranscoder.ListPipelines
    (
    -- * Request
      ListPipelines
    -- ** Request constructor
    , listPipelines
    -- ** Request lenses
    , lpAscending
    , lpPageToken

    -- * Response
    , ListPipelinesResponse
    -- ** Response constructor
    , listPipelinesResponse
    -- ** Response lenses
    , lprNextPageToken
    , lprPipelines
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.Pagers
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listPipelines' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpAscending'
--
-- * 'lpPageToken'
data ListPipelines = ListPipelines'{_lpAscending :: Maybe Text, _lpPageToken :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListPipelines' smart constructor.
listPipelines :: ListPipelines
listPipelines = ListPipelines'{_lpAscending = Nothing, _lpPageToken = Nothing};

-- | To list pipelines in chronological order by the date and time that they
-- were created, enter @true@. To list pipelines in reverse chronological
-- order, enter @false@.
lpAscending :: Lens' ListPipelines (Maybe Text)
lpAscending = lens _lpAscending (\ s a -> s{_lpAscending = a});

-- | When Elastic Transcoder returns more than one page of results, use
-- @pageToken@ in subsequent @GET@ requests to get each successive page of
-- results.
lpPageToken :: Lens' ListPipelines (Maybe Text)
lpPageToken = lens _lpPageToken (\ s a -> s{_lpPageToken = a});

instance AWSPager ListPipelines where
        page rq rs
          | stop (rs ^. lprNextPageToken) = Nothing
          | otherwise =
            rq & lpPageToken ?~ rs ^. lprNextPageToken

instance AWSRequest ListPipelines where
        type Sv ListPipelines = ElasticTranscoder
        type Rs ListPipelines = ListPipelinesResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 ListPipelinesResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "Pipelines" .!@ mempty))

instance ToHeaders ListPipelines where
        toHeaders = const mempty

instance ToPath ListPipelines where
        toPath = const "/2012-09-25/pipelines"

instance ToQuery ListPipelines where
        toQuery ListPipelines'{..}
          = mconcat
              ["Ascending" =: _lpAscending,
               "PageToken" =: _lpPageToken]

-- | /See:/ 'listPipelinesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lprNextPageToken'
--
-- * 'lprPipelines'
data ListPipelinesResponse = ListPipelinesResponse'{_lprNextPageToken :: Maybe Text, _lprPipelines :: Maybe [Pipeline]} deriving (Eq, Read, Show)

-- | 'ListPipelinesResponse' smart constructor.
listPipelinesResponse :: ListPipelinesResponse
listPipelinesResponse = ListPipelinesResponse'{_lprNextPageToken = Nothing, _lprPipelines = Nothing};

-- | A value that you use to access the second and subsequent pages of
-- results, if any. When the pipelines fit on one page or when you\'ve
-- reached the last page of results, the value of @NextPageToken@ is
-- @null@.
lprNextPageToken :: Lens' ListPipelinesResponse (Maybe Text)
lprNextPageToken = lens _lprNextPageToken (\ s a -> s{_lprNextPageToken = a});

-- | An array of @Pipeline@ objects.
lprPipelines :: Lens' ListPipelinesResponse [Pipeline]
lprPipelines = lens _lprPipelines (\ s a -> s{_lprPipelines = a}) . _Default;
