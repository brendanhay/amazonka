{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.ListPipelines
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the pipeline identifiers for all active pipelines that you have
-- permission to access.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_ListPipelines.html>
module Network.AWS.DataPipeline.ListPipelines
    (
    -- * Request
      ListPipelines
    -- ** Request constructor
    , listPipelines
    -- ** Request lenses
    , lpMarker

    -- * Response
    , ListPipelinesResponse
    -- ** Response constructor
    , listPipelinesResponse
    -- ** Response lenses
    , lprsHasMoreResults
    , lprsMarker
    , lprsStatus
    , lprsPipelineIdList
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for ListPipelines.
--
-- /See:/ 'listPipelines' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpMarker'
newtype ListPipelines = ListPipelines'
    { _lpMarker :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListPipelines' smart constructor.
listPipelines :: ListPipelines
listPipelines =
    ListPipelines'
    { _lpMarker = Nothing
    }

-- | The starting point for the results to be returned. For the first call,
-- this value should be empty. As long as there are more results, continue
-- to call @ListPipelines@ with the marker value from the previous call to
-- retrieve the next set of results.
lpMarker :: Lens' ListPipelines (Maybe Text)
lpMarker = lens _lpMarker (\ s a -> s{_lpMarker = a});

instance AWSPager ListPipelines where
        page rq rs
          | stop (rs ^. lprsHasMoreResults) = Nothing
          | isNothing (rs ^. lprsMarker) = Nothing
          | otherwise =
            Just $ rq & lpMarker .~ rs ^. lprsMarker

instance AWSRequest ListPipelines where
        type Sv ListPipelines = DataPipeline
        type Rs ListPipelines = ListPipelinesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListPipelinesResponse' <$>
                   (x .?> "hasMoreResults") <*> (x .?> "marker") <*>
                     (pure (fromEnum s))
                     <*> (x .?> "pipelineIdList" .!@ mempty))

instance ToHeaders ListPipelines where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.ListPipelines" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListPipelines where
        toJSON ListPipelines'{..}
          = object ["marker" .= _lpMarker]

instance ToPath ListPipelines where
        toPath = const mempty

instance ToQuery ListPipelines where
        toQuery = const mempty

-- | Contains the output of ListPipelines.
--
-- /See:/ 'listPipelinesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lprsHasMoreResults'
--
-- * 'lprsMarker'
--
-- * 'lprsStatus'
--
-- * 'lprsPipelineIdList'
data ListPipelinesResponse = ListPipelinesResponse'
    { _lprsHasMoreResults :: !(Maybe Bool)
    , _lprsMarker         :: !(Maybe Text)
    , _lprsStatus         :: !Int
    , _lprsPipelineIdList :: ![PipelineIdName]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListPipelinesResponse' smart constructor.
listPipelinesResponse :: Int -> ListPipelinesResponse
listPipelinesResponse pStatus_ =
    ListPipelinesResponse'
    { _lprsHasMoreResults = Nothing
    , _lprsMarker = Nothing
    , _lprsStatus = pStatus_
    , _lprsPipelineIdList = mempty
    }

-- | Indicates whether there are more results that can be obtained by a
-- subsequent call.
lprsHasMoreResults :: Lens' ListPipelinesResponse (Maybe Bool)
lprsHasMoreResults = lens _lprsHasMoreResults (\ s a -> s{_lprsHasMoreResults = a});

-- | The starting point for the next page of results. To view the next page
-- of results, call @ListPipelinesOutput@ again with this marker value. If
-- the value is null, there are no more results.
lprsMarker :: Lens' ListPipelinesResponse (Maybe Text)
lprsMarker = lens _lprsMarker (\ s a -> s{_lprsMarker = a});

-- | FIXME: Undocumented member.
lprsStatus :: Lens' ListPipelinesResponse Int
lprsStatus = lens _lprsStatus (\ s a -> s{_lprsStatus = a});

-- | The pipeline identifiers. If you require additional information about
-- the pipelines, you can use these identifiers to call DescribePipelines
-- and GetPipelineDefinition.
lprsPipelineIdList :: Lens' ListPipelinesResponse [PipelineIdName]
lprsPipelineIdList = lens _lprsPipelineIdList (\ s a -> s{_lprsPipelineIdList = a}) . _Coerce;
