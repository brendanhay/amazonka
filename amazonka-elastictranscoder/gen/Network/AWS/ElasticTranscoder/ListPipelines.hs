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
-- Module      : Network.AWS.ElasticTranscoder.ListPipelines
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ListPipelines operation gets a list of the pipelines associated with
-- the current AWS account.
--
-- /See:/ <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/ListPipelines.html AWS API Reference> for ListPipelines.
module Network.AWS.ElasticTranscoder.ListPipelines
    (
    -- * Creating a Request
      ListPipelines
    , listPipelines
    -- * Request Lenses
    , lpAscending
    , lpPageToken

    -- * Destructuring the Response
    , ListPipelinesResponse
    , listPipelinesResponse
    -- * Response Lenses
    , lprsNextPageToken
    , lprsPipelines
    , lprsStatus
    ) where

import           Network.AWS.ElasticTranscoder.Types
import           Network.AWS.ElasticTranscoder.Types.Product
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
-- * 'lpAscending'
--
-- * 'lpPageToken'
data ListPipelines = ListPipelines'
    { _lpAscending :: !(Maybe Text)
    , _lpPageToken :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListPipelines' smart constructor.
listPipelines :: ListPipelines
listPipelines =
    ListPipelines'
    { _lpAscending = Nothing
    , _lpPageToken = Nothing
    }

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
          | stop (rs ^. lprsNextPageToken) = Nothing
          | stop (rs ^. lprsPipelines) = Nothing
          | otherwise =
            Just $ rq & lpPageToken .~ rs ^. lprsNextPageToken

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
              ["Ascending" =: _lpAscending,
               "PageToken" =: _lpPageToken]

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
listPipelinesResponse pStatus_ =
    ListPipelinesResponse'
    { _lprsNextPageToken = Nothing
    , _lprsPipelines = Nothing
    , _lprsStatus = pStatus_
    }

-- | A value that you use to access the second and subsequent pages of
-- results, if any. When the pipelines fit on one page or when you\'ve
-- reached the last page of results, the value of @NextPageToken@ is
-- @null@.
lprsNextPageToken :: Lens' ListPipelinesResponse (Maybe Text)
lprsNextPageToken = lens _lprsNextPageToken (\ s a -> s{_lprsNextPageToken = a});

-- | An array of @Pipeline@ objects.
lprsPipelines :: Lens' ListPipelinesResponse [Pipeline]
lprsPipelines = lens _lprsPipelines (\ s a -> s{_lprsPipelines = a}) . _Default . _Coerce;

-- | Undocumented member.
lprsStatus :: Lens' ListPipelinesResponse Int
lprsStatus = lens _lprsStatus (\ s a -> s{_lprsStatus = a});
