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
-- Module      : Network.AWS.XRay.GetServiceGraph
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a document that describes services that process incoming requests, and downstream services that they call as a result. Root services process incoming requests and make calls to downstream services. Root services are applications that use the AWS X-Ray SDK. Downstream services can be other applications, AWS resources, HTTP web APIs, or SQL databases.
--
--
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetServiceGraph
    (
    -- * Creating a Request
      getServiceGraph
    , GetServiceGraph
    -- * Request Lenses
    , gsgNextToken
    , gsgStartTime
    , gsgEndTime

    -- * Destructuring the Response
    , getServiceGraphResponse
    , GetServiceGraphResponse
    -- * Response Lenses
    , gsgrsStartTime
    , gsgrsNextToken
    , gsgrsEndTime
    , gsgrsServices
    , gsgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types
import Network.AWS.XRay.Types.Product

-- | /See:/ 'getServiceGraph' smart constructor.
data GetServiceGraph = GetServiceGraph'
  { _gsgNextToken :: !(Maybe Text)
  , _gsgStartTime :: !POSIX
  , _gsgEndTime   :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetServiceGraph' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsgNextToken' - Pagination token. Not used.
--
-- * 'gsgStartTime' - The start of the time frame for which to generate a graph.
--
-- * 'gsgEndTime' - The end of the time frame for which to generate a graph.
getServiceGraph
    :: UTCTime -- ^ 'gsgStartTime'
    -> UTCTime -- ^ 'gsgEndTime'
    -> GetServiceGraph
getServiceGraph pStartTime_ pEndTime_ =
  GetServiceGraph'
    { _gsgNextToken = Nothing
    , _gsgStartTime = _Time # pStartTime_
    , _gsgEndTime = _Time # pEndTime_
    }


-- | Pagination token. Not used.
gsgNextToken :: Lens' GetServiceGraph (Maybe Text)
gsgNextToken = lens _gsgNextToken (\ s a -> s{_gsgNextToken = a})

-- | The start of the time frame for which to generate a graph.
gsgStartTime :: Lens' GetServiceGraph UTCTime
gsgStartTime = lens _gsgStartTime (\ s a -> s{_gsgStartTime = a}) . _Time

-- | The end of the time frame for which to generate a graph.
gsgEndTime :: Lens' GetServiceGraph UTCTime
gsgEndTime = lens _gsgEndTime (\ s a -> s{_gsgEndTime = a}) . _Time

instance AWSPager GetServiceGraph where
        page rq rs
          | stop (rs ^. gsgrsNextToken) = Nothing
          | stop (rs ^. gsgrsServices) = Nothing
          | otherwise =
            Just $ rq & gsgNextToken .~ rs ^. gsgrsNextToken

instance AWSRequest GetServiceGraph where
        type Rs GetServiceGraph = GetServiceGraphResponse
        request = postJSON xRay
        response
          = receiveJSON
              (\ s h x ->
                 GetServiceGraphResponse' <$>
                   (x .?> "StartTime") <*> (x .?> "NextToken") <*>
                     (x .?> "EndTime")
                     <*> (x .?> "Services" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetServiceGraph where

instance NFData GetServiceGraph where

instance ToHeaders GetServiceGraph where
        toHeaders = const mempty

instance ToJSON GetServiceGraph where
        toJSON GetServiceGraph'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gsgNextToken,
                  Just ("StartTime" .= _gsgStartTime),
                  Just ("EndTime" .= _gsgEndTime)])

instance ToPath GetServiceGraph where
        toPath = const "/ServiceGraph"

instance ToQuery GetServiceGraph where
        toQuery = const mempty

-- | /See:/ 'getServiceGraphResponse' smart constructor.
data GetServiceGraphResponse = GetServiceGraphResponse'
  { _gsgrsStartTime      :: !(Maybe POSIX)
  , _gsgrsNextToken      :: !(Maybe Text)
  , _gsgrsEndTime        :: !(Maybe POSIX)
  , _gsgrsServices       :: !(Maybe [ServiceInfo])
  , _gsgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetServiceGraphResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsgrsStartTime' - The start of the time frame for which the graph was generated.
--
-- * 'gsgrsNextToken' - Pagination token. Not used.
--
-- * 'gsgrsEndTime' - The end of the time frame for which the graph was generated.
--
-- * 'gsgrsServices' - The services that have processed a traced request during the specified time frame.
--
-- * 'gsgrsResponseStatus' - -- | The response status code.
getServiceGraphResponse
    :: Int -- ^ 'gsgrsResponseStatus'
    -> GetServiceGraphResponse
getServiceGraphResponse pResponseStatus_ =
  GetServiceGraphResponse'
    { _gsgrsStartTime = Nothing
    , _gsgrsNextToken = Nothing
    , _gsgrsEndTime = Nothing
    , _gsgrsServices = Nothing
    , _gsgrsResponseStatus = pResponseStatus_
    }


-- | The start of the time frame for which the graph was generated.
gsgrsStartTime :: Lens' GetServiceGraphResponse (Maybe UTCTime)
gsgrsStartTime = lens _gsgrsStartTime (\ s a -> s{_gsgrsStartTime = a}) . mapping _Time

-- | Pagination token. Not used.
gsgrsNextToken :: Lens' GetServiceGraphResponse (Maybe Text)
gsgrsNextToken = lens _gsgrsNextToken (\ s a -> s{_gsgrsNextToken = a})

-- | The end of the time frame for which the graph was generated.
gsgrsEndTime :: Lens' GetServiceGraphResponse (Maybe UTCTime)
gsgrsEndTime = lens _gsgrsEndTime (\ s a -> s{_gsgrsEndTime = a}) . mapping _Time

-- | The services that have processed a traced request during the specified time frame.
gsgrsServices :: Lens' GetServiceGraphResponse [ServiceInfo]
gsgrsServices = lens _gsgrsServices (\ s a -> s{_gsgrsServices = a}) . _Default . _Coerce

-- | -- | The response status code.
gsgrsResponseStatus :: Lens' GetServiceGraphResponse Int
gsgrsResponseStatus = lens _gsgrsResponseStatus (\ s a -> s{_gsgrsResponseStatus = a})

instance NFData GetServiceGraphResponse where
