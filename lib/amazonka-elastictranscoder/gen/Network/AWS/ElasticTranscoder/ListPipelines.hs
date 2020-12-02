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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ListPipelines operation gets a list of the pipelines associated with the current AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ElasticTranscoder.ListPipelines
    (
    -- * Creating a Request
      listPipelines
    , ListPipelines
    -- * Request Lenses
    , lpAscending
    , lpPageToken

    -- * Destructuring the Response
    , listPipelinesResponse
    , ListPipelinesResponse
    -- * Response Lenses
    , lprsNextPageToken
    , lprsPipelines
    , lprsResponseStatus
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.ElasticTranscoder.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The @ListPipelineRequest@ structure.
--
--
--
-- /See:/ 'listPipelines' smart constructor.
data ListPipelines = ListPipelines'
  { _lpAscending :: !(Maybe Text)
  , _lpPageToken :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPipelines' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpAscending' - To list pipelines in chronological order by the date and time that they were created, enter @true@ . To list pipelines in reverse chronological order, enter @false@ .
--
-- * 'lpPageToken' - When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
listPipelines
    :: ListPipelines
listPipelines = ListPipelines' {_lpAscending = Nothing, _lpPageToken = Nothing}


-- | To list pipelines in chronological order by the date and time that they were created, enter @true@ . To list pipelines in reverse chronological order, enter @false@ .
lpAscending :: Lens' ListPipelines (Maybe Text)
lpAscending = lens _lpAscending (\ s a -> s{_lpAscending = a})

-- | When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
lpPageToken :: Lens' ListPipelines (Maybe Text)
lpPageToken = lens _lpPageToken (\ s a -> s{_lpPageToken = a})

instance AWSPager ListPipelines where
        page rq rs
          | stop (rs ^. lprsNextPageToken) = Nothing
          | stop (rs ^. lprsPipelines) = Nothing
          | otherwise =
            Just $ rq & lpPageToken .~ rs ^. lprsNextPageToken

instance AWSRequest ListPipelines where
        type Rs ListPipelines = ListPipelinesResponse
        request = get elasticTranscoder
        response
          = receiveJSON
              (\ s h x ->
                 ListPipelinesResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "Pipelines" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListPipelines where

instance NFData ListPipelines where

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
--
--
-- /See:/ 'listPipelinesResponse' smart constructor.
data ListPipelinesResponse = ListPipelinesResponse'
  { _lprsNextPageToken  :: !(Maybe Text)
  , _lprsPipelines      :: !(Maybe [Pipeline])
  , _lprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPipelinesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsNextPageToken' - A value that you use to access the second and subsequent pages of results, if any. When the pipelines fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
--
-- * 'lprsPipelines' - An array of @Pipeline@ objects.
--
-- * 'lprsResponseStatus' - -- | The response status code.
listPipelinesResponse
    :: Int -- ^ 'lprsResponseStatus'
    -> ListPipelinesResponse
listPipelinesResponse pResponseStatus_ =
  ListPipelinesResponse'
    { _lprsNextPageToken = Nothing
    , _lprsPipelines = Nothing
    , _lprsResponseStatus = pResponseStatus_
    }


-- | A value that you use to access the second and subsequent pages of results, if any. When the pipelines fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
lprsNextPageToken :: Lens' ListPipelinesResponse (Maybe Text)
lprsNextPageToken = lens _lprsNextPageToken (\ s a -> s{_lprsNextPageToken = a})

-- | An array of @Pipeline@ objects.
lprsPipelines :: Lens' ListPipelinesResponse [Pipeline]
lprsPipelines = lens _lprsPipelines (\ s a -> s{_lprsPipelines = a}) . _Default . _Coerce

-- | -- | The response status code.
lprsResponseStatus :: Lens' ListPipelinesResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\ s a -> s{_lprsResponseStatus = a})

instance NFData ListPipelinesResponse where
