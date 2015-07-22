{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.ListPipelines
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The ListPipelines operation gets a list of the pipelines associated with
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
    , lprqAscending
    , lprqPageToken

    -- * Response
    , ListPipelinesResponse
    -- ** Response constructor
    , listPipelinesResponse
    -- ** Response lenses
    , lprsNextPageToken
    , lprsPipelines
    , lprsStatus
    ) where

import           Network.AWS.ElasticTranscoder.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The @ListPipelineRequest@ structure.
--
-- /See:/ 'listPipelines' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lprqAscending'
--
-- * 'lprqPageToken'
data ListPipelines = ListPipelines'
    { _lprqAscending :: !(Maybe Text)
    , _lprqPageToken :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListPipelines' smart constructor.
listPipelines :: ListPipelines
listPipelines =
    ListPipelines'
    { _lprqAscending = Nothing
    , _lprqPageToken = Nothing
    }

-- | To list pipelines in chronological order by the date and time that they
-- were created, enter @true@. To list pipelines in reverse chronological
-- order, enter @false@.
lprqAscending :: Lens' ListPipelines (Maybe Text)
lprqAscending = lens _lprqAscending (\ s a -> s{_lprqAscending = a});

-- | When Elastic Transcoder returns more than one page of results, use
-- @pageToken@ in subsequent @GET@ requests to get each successive page of
-- results.
lprqPageToken :: Lens' ListPipelines (Maybe Text)
lprqPageToken = lens _lprqPageToken (\ s a -> s{_lprqPageToken = a});

instance AWSPager ListPipelines where
        page rq rs
          | stop (rs ^. lprsNextPageToken) = Nothing
          | stop (rs ^. lprsPipelines) = Nothing
          | otherwise =
            Just $ rq & lprqPageToken .~ rs ^. lprsNextPageToken

instance AWSRequest ListPipelines where
        type Sv ListPipelines = ElasticTranscoder
        type Rs ListPipelines = ListPipelinesResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 ListPipelinesResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "Pipelines" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListPipelines where
        toHeaders = const mempty

instance ToPath ListPipelines where
        toPath = const "/2012-09-25/pipelines"

instance ToQuery ListPipelines where
        toQuery ListPipelines'{..}
          = mconcat
              ["Ascending" =: _lprqAscending,
               "PageToken" =: _lprqPageToken]

-- | A list of the pipelines associated with the current AWS account.
--
-- /See:/ 'listPipelinesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lprsNextPageToken'
--
-- * 'lprsPipelines'
--
-- * 'lprsStatus'
data ListPipelinesResponse = ListPipelinesResponse'
    { _lprsNextPageToken :: !(Maybe Text)
    , _lprsPipelines     :: !(Maybe [Pipeline])
    , _lprsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListPipelinesResponse' smart constructor.
listPipelinesResponse :: Int -> ListPipelinesResponse
listPipelinesResponse pStatus =
    ListPipelinesResponse'
    { _lprsNextPageToken = Nothing
    , _lprsPipelines = Nothing
    , _lprsStatus = pStatus
    }

-- | A value that you use to access the second and subsequent pages of
-- results, if any. When the pipelines fit on one page or when you\'ve
-- reached the last page of results, the value of @NextPageToken@ is
-- @null@.
lprsNextPageToken :: Lens' ListPipelinesResponse (Maybe Text)
lprsNextPageToken = lens _lprsNextPageToken (\ s a -> s{_lprsNextPageToken = a});

-- | An array of @Pipeline@ objects.
lprsPipelines :: Lens' ListPipelinesResponse [Pipeline]
lprsPipelines = lens _lprsPipelines (\ s a -> s{_lprsPipelines = a}) . _Default;

-- | FIXME: Undocumented member.
lprsStatus :: Lens' ListPipelinesResponse Int
lprsStatus = lens _lprsStatus (\ s a -> s{_lprsStatus = a});
