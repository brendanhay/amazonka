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
-- Module      : Network.AWS.CloudFront.UpdateStreamingDistribution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a streaming distribution.
--
--
module Network.AWS.CloudFront.UpdateStreamingDistribution
    (
    -- * Creating a Request
      updateStreamingDistribution
    , UpdateStreamingDistribution
    -- * Request Lenses
    , usdIfMatch
    , usdStreamingDistributionConfig
    , usdId

    -- * Destructuring the Response
    , updateStreamingDistributionResponse
    , UpdateStreamingDistributionResponse
    -- * Response Lenses
    , usdrsETag
    , usdrsStreamingDistribution
    , usdrsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to update a streaming distribution.
--
--
--
-- /See:/ 'updateStreamingDistribution' smart constructor.
data UpdateStreamingDistribution = UpdateStreamingDistribution'
  { _usdIfMatch                     :: !(Maybe Text)
  , _usdStreamingDistributionConfig :: !StreamingDistributionConfig
  , _usdId                          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStreamingDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usdIfMatch' - The value of the @ETag@ header that you received when retrieving the streaming distribution's configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'usdStreamingDistributionConfig' - The streaming distribution's configuration information.
--
-- * 'usdId' - The streaming distribution's id.
updateStreamingDistribution
    :: StreamingDistributionConfig -- ^ 'usdStreamingDistributionConfig'
    -> Text -- ^ 'usdId'
    -> UpdateStreamingDistribution
updateStreamingDistribution pStreamingDistributionConfig_ pId_ =
  UpdateStreamingDistribution'
    { _usdIfMatch = Nothing
    , _usdStreamingDistributionConfig = pStreamingDistributionConfig_
    , _usdId = pId_
    }


-- | The value of the @ETag@ header that you received when retrieving the streaming distribution's configuration. For example: @E2QWRUHAPOMQZL@ .
usdIfMatch :: Lens' UpdateStreamingDistribution (Maybe Text)
usdIfMatch = lens _usdIfMatch (\ s a -> s{_usdIfMatch = a})

-- | The streaming distribution's configuration information.
usdStreamingDistributionConfig :: Lens' UpdateStreamingDistribution StreamingDistributionConfig
usdStreamingDistributionConfig = lens _usdStreamingDistributionConfig (\ s a -> s{_usdStreamingDistributionConfig = a})

-- | The streaming distribution's id.
usdId :: Lens' UpdateStreamingDistribution Text
usdId = lens _usdId (\ s a -> s{_usdId = a})

instance AWSRequest UpdateStreamingDistribution where
        type Rs UpdateStreamingDistribution =
             UpdateStreamingDistributionResponse
        request = putXML cloudFront
        response
          = receiveXML
              (\ s h x ->
                 UpdateStreamingDistributionResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (pure (fromEnum s)))

instance Hashable UpdateStreamingDistribution where

instance NFData UpdateStreamingDistribution where

instance ToElement UpdateStreamingDistribution where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2017-10-30/}StreamingDistributionConfig"
              .
              _usdStreamingDistributionConfig

instance ToHeaders UpdateStreamingDistribution where
        toHeaders UpdateStreamingDistribution'{..}
          = mconcat ["If-Match" =# _usdIfMatch]

instance ToPath UpdateStreamingDistribution where
        toPath UpdateStreamingDistribution'{..}
          = mconcat
              ["/2017-10-30/streaming-distribution/", toBS _usdId,
               "/config"]

instance ToQuery UpdateStreamingDistribution where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
--
--
-- /See:/ 'updateStreamingDistributionResponse' smart constructor.
data UpdateStreamingDistributionResponse = UpdateStreamingDistributionResponse'
  { _usdrsETag                  :: !(Maybe Text)
  , _usdrsStreamingDistribution :: !(Maybe StreamingDistribution)
  , _usdrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStreamingDistributionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usdrsETag' - The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'usdrsStreamingDistribution' - The streaming distribution's information.
--
-- * 'usdrsResponseStatus' - -- | The response status code.
updateStreamingDistributionResponse
    :: Int -- ^ 'usdrsResponseStatus'
    -> UpdateStreamingDistributionResponse
updateStreamingDistributionResponse pResponseStatus_ =
  UpdateStreamingDistributionResponse'
    { _usdrsETag = Nothing
    , _usdrsStreamingDistribution = Nothing
    , _usdrsResponseStatus = pResponseStatus_
    }


-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
usdrsETag :: Lens' UpdateStreamingDistributionResponse (Maybe Text)
usdrsETag = lens _usdrsETag (\ s a -> s{_usdrsETag = a})

-- | The streaming distribution's information.
usdrsStreamingDistribution :: Lens' UpdateStreamingDistributionResponse (Maybe StreamingDistribution)
usdrsStreamingDistribution = lens _usdrsStreamingDistribution (\ s a -> s{_usdrsStreamingDistribution = a})

-- | -- | The response status code.
usdrsResponseStatus :: Lens' UpdateStreamingDistributionResponse Int
usdrsResponseStatus = lens _usdrsResponseStatus (\ s a -> s{_usdrsResponseStatus = a})

instance NFData UpdateStreamingDistributionResponse
         where
