{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateStreamingDistribution
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Update a streaming distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/UpdateStreamingDistribution.html>
module Network.AWS.CloudFront.UpdateStreamingDistribution
    (
    -- * Request
      UpdateStreamingDistribution
    -- ** Request constructor
    , updateStreamingDistribution
    -- ** Request lenses
    , usdIfMatch
    , usdStreamingDistributionConfig
    , usdId

    -- * Response
    , UpdateStreamingDistributionResponse
    -- ** Response constructor
    , updateStreamingDistributionResponse
    -- ** Response lenses
    , usdrsETag
    , usdrsStreamingDistribution
    , usdrsStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to update a streaming distribution.
--
-- /See:/ 'updateStreamingDistribution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usdIfMatch'
--
-- * 'usdStreamingDistributionConfig'
--
-- * 'usdId'
data UpdateStreamingDistribution = UpdateStreamingDistribution'
    { _usdIfMatch                     :: !(Maybe Text)
    , _usdStreamingDistributionConfig :: !StreamingDistributionConfig
    , _usdId                          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateStreamingDistribution' smart constructor.
updateStreamingDistribution :: StreamingDistributionConfig -> Text -> UpdateStreamingDistribution
updateStreamingDistribution pStreamingDistributionConfig_ pId_ =
    UpdateStreamingDistribution'
    { _usdIfMatch = Nothing
    , _usdStreamingDistributionConfig = pStreamingDistributionConfig_
    , _usdId = pId_
    }

-- | The value of the ETag header you received when retrieving the streaming
-- distribution\'s configuration. For example: E2QWRUHAPOMQZL.
usdIfMatch :: Lens' UpdateStreamingDistribution (Maybe Text)
usdIfMatch = lens _usdIfMatch (\ s a -> s{_usdIfMatch = a});

-- | The streaming distribution\'s configuration information.
usdStreamingDistributionConfig :: Lens' UpdateStreamingDistribution StreamingDistributionConfig
usdStreamingDistributionConfig = lens _usdStreamingDistributionConfig (\ s a -> s{_usdStreamingDistributionConfig = a});

-- | The streaming distribution\'s id.
usdId :: Lens' UpdateStreamingDistribution Text
usdId = lens _usdId (\ s a -> s{_usdId = a});

instance AWSRequest UpdateStreamingDistribution where
        type Sv UpdateStreamingDistribution = CloudFront
        type Rs UpdateStreamingDistribution =
             UpdateStreamingDistributionResponse
        request = putXML
        response
          = receiveXML
              (\ s h x ->
                 UpdateStreamingDistributionResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (pure (fromEnum s)))

instance ToElement UpdateStreamingDistribution where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2015-04-17/}StreamingDistributionConfig"
              .
              _usdStreamingDistributionConfig

instance ToHeaders UpdateStreamingDistribution where
        toHeaders UpdateStreamingDistribution'{..}
          = mconcat ["If-Match" =# _usdIfMatch]

instance ToPath UpdateStreamingDistribution where
        toPath UpdateStreamingDistribution'{..}
          = mconcat
              ["/2015-04-17/streaming-distribution/",
               toPath _usdId, "/config"]

instance ToQuery UpdateStreamingDistribution where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'updateStreamingDistributionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usdrsETag'
--
-- * 'usdrsStreamingDistribution'
--
-- * 'usdrsStatus'
data UpdateStreamingDistributionResponse = UpdateStreamingDistributionResponse'
    { _usdrsETag                  :: !(Maybe Text)
    , _usdrsStreamingDistribution :: !(Maybe StreamingDistribution)
    , _usdrsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateStreamingDistributionResponse' smart constructor.
updateStreamingDistributionResponse :: Int -> UpdateStreamingDistributionResponse
updateStreamingDistributionResponse pStatus_ =
    UpdateStreamingDistributionResponse'
    { _usdrsETag = Nothing
    , _usdrsStreamingDistribution = Nothing
    , _usdrsStatus = pStatus_
    }

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
usdrsETag :: Lens' UpdateStreamingDistributionResponse (Maybe Text)
usdrsETag = lens _usdrsETag (\ s a -> s{_usdrsETag = a});

-- | The streaming distribution\'s information.
usdrsStreamingDistribution :: Lens' UpdateStreamingDistributionResponse (Maybe StreamingDistribution)
usdrsStreamingDistribution = lens _usdrsStreamingDistribution (\ s a -> s{_usdrsStreamingDistribution = a});

-- | FIXME: Undocumented member.
usdrsStatus :: Lens' UpdateStreamingDistributionResponse Int
usdrsStatus = lens _usdrsStatus (\ s a -> s{_usdrsStatus = a});
