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
-- Module      : Network.AWS.XRay.GetTraceGraph
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a service graph for one or more specific trace IDs.
--
--
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetTraceGraph
    (
    -- * Creating a Request
      getTraceGraph
    , GetTraceGraph
    -- * Request Lenses
    , gtgNextToken
    , gtgTraceIds

    -- * Destructuring the Response
    , getTraceGraphResponse
    , GetTraceGraphResponse
    -- * Response Lenses
    , gtgrsNextToken
    , gtgrsServices
    , gtgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types
import Network.AWS.XRay.Types.Product

-- | /See:/ 'getTraceGraph' smart constructor.
data GetTraceGraph = GetTraceGraph'
  { _gtgNextToken :: !(Maybe Text)
  , _gtgTraceIds  :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTraceGraph' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtgNextToken' - Pagination token. Not used.
--
-- * 'gtgTraceIds' - Trace IDs of requests for which to generate a service graph.
getTraceGraph
    :: GetTraceGraph
getTraceGraph = GetTraceGraph' {_gtgNextToken = Nothing, _gtgTraceIds = mempty}


-- | Pagination token. Not used.
gtgNextToken :: Lens' GetTraceGraph (Maybe Text)
gtgNextToken = lens _gtgNextToken (\ s a -> s{_gtgNextToken = a})

-- | Trace IDs of requests for which to generate a service graph.
gtgTraceIds :: Lens' GetTraceGraph [Text]
gtgTraceIds = lens _gtgTraceIds (\ s a -> s{_gtgTraceIds = a}) . _Coerce

instance AWSPager GetTraceGraph where
        page rq rs
          | stop (rs ^. gtgrsNextToken) = Nothing
          | stop (rs ^. gtgrsServices) = Nothing
          | otherwise =
            Just $ rq & gtgNextToken .~ rs ^. gtgrsNextToken

instance AWSRequest GetTraceGraph where
        type Rs GetTraceGraph = GetTraceGraphResponse
        request = postJSON xRay
        response
          = receiveJSON
              (\ s h x ->
                 GetTraceGraphResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Services" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetTraceGraph where

instance NFData GetTraceGraph where

instance ToHeaders GetTraceGraph where
        toHeaders = const mempty

instance ToJSON GetTraceGraph where
        toJSON GetTraceGraph'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gtgNextToken,
                  Just ("TraceIds" .= _gtgTraceIds)])

instance ToPath GetTraceGraph where
        toPath = const "/TraceGraph"

instance ToQuery GetTraceGraph where
        toQuery = const mempty

-- | /See:/ 'getTraceGraphResponse' smart constructor.
data GetTraceGraphResponse = GetTraceGraphResponse'
  { _gtgrsNextToken      :: !(Maybe Text)
  , _gtgrsServices       :: !(Maybe [ServiceInfo])
  , _gtgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTraceGraphResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtgrsNextToken' - Pagination token. Not used.
--
-- * 'gtgrsServices' - The services that have processed one of the specified requests.
--
-- * 'gtgrsResponseStatus' - -- | The response status code.
getTraceGraphResponse
    :: Int -- ^ 'gtgrsResponseStatus'
    -> GetTraceGraphResponse
getTraceGraphResponse pResponseStatus_ =
  GetTraceGraphResponse'
    { _gtgrsNextToken = Nothing
    , _gtgrsServices = Nothing
    , _gtgrsResponseStatus = pResponseStatus_
    }


-- | Pagination token. Not used.
gtgrsNextToken :: Lens' GetTraceGraphResponse (Maybe Text)
gtgrsNextToken = lens _gtgrsNextToken (\ s a -> s{_gtgrsNextToken = a})

-- | The services that have processed one of the specified requests.
gtgrsServices :: Lens' GetTraceGraphResponse [ServiceInfo]
gtgrsServices = lens _gtgrsServices (\ s a -> s{_gtgrsServices = a}) . _Default . _Coerce

-- | -- | The response status code.
gtgrsResponseStatus :: Lens' GetTraceGraphResponse Int
gtgrsResponseStatus = lens _gtgrsResponseStatus (\ s a -> s{_gtgrsResponseStatus = a})

instance NFData GetTraceGraphResponse where
