{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateStreamingDistribution
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Create a new streaming distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/CreateStreamingDistribution.html>
module Network.AWS.CloudFront.CreateStreamingDistribution
    (
    -- * Request
      CreateStreamingDistribution
    -- ** Request constructor
    , createStreamingDistribution
    -- ** Request lenses
    , csdrqStreamingDistributionConfig

    -- * Response
    , CreateStreamingDistributionResponse
    -- ** Response constructor
    , createStreamingDistributionResponse
    -- ** Response lenses
    , csdrsETag
    , csdrsLocation
    , csdrsStreamingDistribution
    , csdrsStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to create a new streaming distribution.
--
-- /See:/ 'createStreamingDistribution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdrqStreamingDistributionConfig'
newtype CreateStreamingDistribution = CreateStreamingDistribution'
    { _csdrqStreamingDistributionConfig :: StreamingDistributionConfig
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateStreamingDistribution' smart constructor.
createStreamingDistribution :: StreamingDistributionConfig -> CreateStreamingDistribution
createStreamingDistribution pStreamingDistributionConfig =
    CreateStreamingDistribution'
    { _csdrqStreamingDistributionConfig = pStreamingDistributionConfig
    }

-- | The streaming distribution\'s configuration information.
csdrqStreamingDistributionConfig :: Lens' CreateStreamingDistribution StreamingDistributionConfig
csdrqStreamingDistributionConfig = lens _csdrqStreamingDistributionConfig (\ s a -> s{_csdrqStreamingDistributionConfig = a});

instance AWSRequest CreateStreamingDistribution where
        type Sv CreateStreamingDistribution = CloudFront
        type Rs CreateStreamingDistribution =
             CreateStreamingDistributionResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 CreateStreamingDistributionResponse' <$>
                   (h .#? "ETag") <*> (h .#? "Location") <*>
                     (parseXML x)
                     <*> (pure (fromEnum s)))

instance ToElement CreateStreamingDistribution where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2015-04-17/}StreamingDistributionConfig"
              .
              _csdrqStreamingDistributionConfig

instance ToHeaders CreateStreamingDistribution where
        toHeaders = const mempty

instance ToPath CreateStreamingDistribution where
        toPath = const "/2015-04-17/streaming-distribution"

instance ToQuery CreateStreamingDistribution where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'createStreamingDistributionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdrsETag'
--
-- * 'csdrsLocation'
--
-- * 'csdrsStreamingDistribution'
--
-- * 'csdrsStatus'
data CreateStreamingDistributionResponse = CreateStreamingDistributionResponse'
    { _csdrsETag                  :: !(Maybe Text)
    , _csdrsLocation              :: !(Maybe Text)
    , _csdrsStreamingDistribution :: !(Maybe StreamingDistribution)
    , _csdrsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateStreamingDistributionResponse' smart constructor.
createStreamingDistributionResponse :: Int -> CreateStreamingDistributionResponse
createStreamingDistributionResponse pStatus =
    CreateStreamingDistributionResponse'
    { _csdrsETag = Nothing
    , _csdrsLocation = Nothing
    , _csdrsStreamingDistribution = Nothing
    , _csdrsStatus = pStatus
    }

-- | The current version of the streaming distribution created.
csdrsETag :: Lens' CreateStreamingDistributionResponse (Maybe Text)
csdrsETag = lens _csdrsETag (\ s a -> s{_csdrsETag = a});

-- | The fully qualified URI of the new streaming distribution resource just
-- created. For example:
-- https:\/\/cloudfront.amazonaws.com\/2010-11-01\/streaming-distribution\/EGTXBD79H29TRA8.
csdrsLocation :: Lens' CreateStreamingDistributionResponse (Maybe Text)
csdrsLocation = lens _csdrsLocation (\ s a -> s{_csdrsLocation = a});

-- | The streaming distribution\'s information.
csdrsStreamingDistribution :: Lens' CreateStreamingDistributionResponse (Maybe StreamingDistribution)
csdrsStreamingDistribution = lens _csdrsStreamingDistribution (\ s a -> s{_csdrsStreamingDistribution = a});

-- | FIXME: Undocumented member.
csdrsStatus :: Lens' CreateStreamingDistributionResponse Int
csdrsStatus = lens _csdrsStatus (\ s a -> s{_csdrsStatus = a});
