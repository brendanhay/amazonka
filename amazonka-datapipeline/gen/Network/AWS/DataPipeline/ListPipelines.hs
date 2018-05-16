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
-- Module      : Network.AWS.DataPipeline.ListPipelines
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the pipeline identifiers for all active pipelines that you have permission to access.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DataPipeline.ListPipelines
    (
    -- * Creating a Request
      listPipelines
    , ListPipelines
    -- * Request Lenses
    , lpMarker

    -- * Destructuring the Response
    , listPipelinesResponse
    , ListPipelinesResponse
    -- * Response Lenses
    , lprsHasMoreResults
    , lprsMarker
    , lprsResponseStatus
    , lprsPipelineIdList
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.DataPipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ListPipelines.
--
--
--
-- /See:/ 'listPipelines' smart constructor.
newtype ListPipelines = ListPipelines'
  { _lpMarker :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPipelines' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpMarker' - The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @ListPipelines@ with the marker value from the previous call to retrieve the next set of results.
listPipelines
    :: ListPipelines
listPipelines = ListPipelines' {_lpMarker = Nothing}


-- | The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @ListPipelines@ with the marker value from the previous call to retrieve the next set of results.
lpMarker :: Lens' ListPipelines (Maybe Text)
lpMarker = lens _lpMarker (\ s a -> s{_lpMarker = a})

instance AWSPager ListPipelines where
        page rq rs
          | stop (rs ^. lprsHasMoreResults) = Nothing
          | isNothing (rs ^. lprsMarker) = Nothing
          | otherwise =
            Just $ rq & lpMarker .~ rs ^. lprsMarker

instance AWSRequest ListPipelines where
        type Rs ListPipelines = ListPipelinesResponse
        request = postJSON dataPipeline
        response
          = receiveJSON
              (\ s h x ->
                 ListPipelinesResponse' <$>
                   (x .?> "hasMoreResults") <*> (x .?> "marker") <*>
                     (pure (fromEnum s))
                     <*> (x .?> "pipelineIdList" .!@ mempty))

instance Hashable ListPipelines where

instance NFData ListPipelines where

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
          = object (catMaybes [("marker" .=) <$> _lpMarker])

instance ToPath ListPipelines where
        toPath = const "/"

instance ToQuery ListPipelines where
        toQuery = const mempty

-- | Contains the output of ListPipelines.
--
--
--
-- /See:/ 'listPipelinesResponse' smart constructor.
data ListPipelinesResponse = ListPipelinesResponse'
  { _lprsHasMoreResults :: !(Maybe Bool)
  , _lprsMarker         :: !(Maybe Text)
  , _lprsResponseStatus :: !Int
  , _lprsPipelineIdList :: ![PipelineIdName]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPipelinesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsHasMoreResults' - Indicates whether there are more results that can be obtained by a subsequent call.
--
-- * 'lprsMarker' - The starting point for the next page of results. To view the next page of results, call @ListPipelinesOutput@ again with this marker value. If the value is null, there are no more results.
--
-- * 'lprsResponseStatus' - -- | The response status code.
--
-- * 'lprsPipelineIdList' - The pipeline identifiers. If you require additional information about the pipelines, you can use these identifiers to call 'DescribePipelines' and 'GetPipelineDefinition' .
listPipelinesResponse
    :: Int -- ^ 'lprsResponseStatus'
    -> ListPipelinesResponse
listPipelinesResponse pResponseStatus_ =
  ListPipelinesResponse'
    { _lprsHasMoreResults = Nothing
    , _lprsMarker = Nothing
    , _lprsResponseStatus = pResponseStatus_
    , _lprsPipelineIdList = mempty
    }


-- | Indicates whether there are more results that can be obtained by a subsequent call.
lprsHasMoreResults :: Lens' ListPipelinesResponse (Maybe Bool)
lprsHasMoreResults = lens _lprsHasMoreResults (\ s a -> s{_lprsHasMoreResults = a})

-- | The starting point for the next page of results. To view the next page of results, call @ListPipelinesOutput@ again with this marker value. If the value is null, there are no more results.
lprsMarker :: Lens' ListPipelinesResponse (Maybe Text)
lprsMarker = lens _lprsMarker (\ s a -> s{_lprsMarker = a})

-- | -- | The response status code.
lprsResponseStatus :: Lens' ListPipelinesResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\ s a -> s{_lprsResponseStatus = a})

-- | The pipeline identifiers. If you require additional information about the pipelines, you can use these identifiers to call 'DescribePipelines' and 'GetPipelineDefinition' .
lprsPipelineIdList :: Lens' ListPipelinesResponse [PipelineIdName]
lprsPipelineIdList = lens _lprsPipelineIdList (\ s a -> s{_lprsPipelineIdList = a}) . _Coerce

instance NFData ListPipelinesResponse where
