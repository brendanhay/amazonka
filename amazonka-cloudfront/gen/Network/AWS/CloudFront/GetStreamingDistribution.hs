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
-- Module      : Network.AWS.CloudFront.GetStreamingDistribution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified RTMP distribution, including the distribution configuration.
--
--
module Network.AWS.CloudFront.GetStreamingDistribution
    (
    -- * Creating a Request
      getStreamingDistribution
    , GetStreamingDistribution
    -- * Request Lenses
    , gsdId

    -- * Destructuring the Response
    , getStreamingDistributionResponse
    , GetStreamingDistributionResponse
    -- * Response Lenses
    , gsdrsETag
    , gsdrsStreamingDistribution
    , gsdrsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to get a streaming distribution's information.
--
--
--
-- /See:/ 'getStreamingDistribution' smart constructor.
newtype GetStreamingDistribution = GetStreamingDistribution'
  { _gsdId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetStreamingDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsdId' - The streaming distribution's ID.
getStreamingDistribution
    :: Text -- ^ 'gsdId'
    -> GetStreamingDistribution
getStreamingDistribution pId_ = GetStreamingDistribution' {_gsdId = pId_}


-- | The streaming distribution's ID.
gsdId :: Lens' GetStreamingDistribution Text
gsdId = lens _gsdId (\ s a -> s{_gsdId = a})

instance AWSRequest GetStreamingDistribution where
        type Rs GetStreamingDistribution =
             GetStreamingDistributionResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 GetStreamingDistributionResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (pure (fromEnum s)))

instance Hashable GetStreamingDistribution where

instance NFData GetStreamingDistribution where

instance ToHeaders GetStreamingDistribution where
        toHeaders = const mempty

instance ToPath GetStreamingDistribution where
        toPath GetStreamingDistribution'{..}
          = mconcat
              ["/2017-10-30/streaming-distribution/", toBS _gsdId]

instance ToQuery GetStreamingDistribution where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
--
--
-- /See:/ 'getStreamingDistributionResponse' smart constructor.
data GetStreamingDistributionResponse = GetStreamingDistributionResponse'
  { _gsdrsETag                  :: !(Maybe Text)
  , _gsdrsStreamingDistribution :: !(Maybe StreamingDistribution)
  , _gsdrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetStreamingDistributionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsdrsETag' - The current version of the streaming distribution's information. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'gsdrsStreamingDistribution' - The streaming distribution's information.
--
-- * 'gsdrsResponseStatus' - -- | The response status code.
getStreamingDistributionResponse
    :: Int -- ^ 'gsdrsResponseStatus'
    -> GetStreamingDistributionResponse
getStreamingDistributionResponse pResponseStatus_ =
  GetStreamingDistributionResponse'
    { _gsdrsETag = Nothing
    , _gsdrsStreamingDistribution = Nothing
    , _gsdrsResponseStatus = pResponseStatus_
    }


-- | The current version of the streaming distribution's information. For example: @E2QWRUHAPOMQZL@ .
gsdrsETag :: Lens' GetStreamingDistributionResponse (Maybe Text)
gsdrsETag = lens _gsdrsETag (\ s a -> s{_gsdrsETag = a})

-- | The streaming distribution's information.
gsdrsStreamingDistribution :: Lens' GetStreamingDistributionResponse (Maybe StreamingDistribution)
gsdrsStreamingDistribution = lens _gsdrsStreamingDistribution (\ s a -> s{_gsdrsStreamingDistribution = a})

-- | -- | The response status code.
gsdrsResponseStatus :: Lens' GetStreamingDistributionResponse Int
gsdrsResponseStatus = lens _gsdrsResponseStatus (\ s a -> s{_gsdrsResponseStatus = a})

instance NFData GetStreamingDistributionResponse
         where
