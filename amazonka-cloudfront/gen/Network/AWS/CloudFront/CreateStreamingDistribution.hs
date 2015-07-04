{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.CloudFront.CreateStreamingDistribution
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Create a new streaming distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/CreateStreamingDistribution.html>
module Network.AWS.CloudFront.CreateStreamingDistribution
    (
    -- * Request
      CreateStreamingDistribution
    -- ** Request constructor
    , createStreamingDistribution
    -- ** Request lenses
    , csdStreamingDistributionConfig

    -- * Response
    , CreateStreamingDistributionResponse
    -- ** Response constructor
    , createStreamingDistributionResponse
    -- ** Response lenses
    , csdrETag
    , csdrLocation
    , csdrStreamingDistribution
    , csdrStatus
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
-- * 'csdStreamingDistributionConfig'
newtype CreateStreamingDistribution = CreateStreamingDistribution'
    { _csdStreamingDistributionConfig :: StreamingDistributionConfig
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateStreamingDistribution' smart constructor.
createStreamingDistribution :: StreamingDistributionConfig -> CreateStreamingDistribution
createStreamingDistribution pStreamingDistributionConfig =
    CreateStreamingDistribution'
    { _csdStreamingDistributionConfig = pStreamingDistributionConfig
    }

-- | The streaming distribution\'s configuration information.
csdStreamingDistributionConfig :: Lens' CreateStreamingDistribution StreamingDistributionConfig
csdStreamingDistributionConfig = lens _csdStreamingDistributionConfig (\ s a -> s{_csdStreamingDistributionConfig = a});

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
                     (x .@? "StreamingDistribution")
                     <*> (pure (fromEnum s)))

instance ToElement CreateStreamingDistribution where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2015-04-17/}StreamingDistributionConfig"
              .
              _csdStreamingDistributionConfig

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
-- * 'csdrETag'
--
-- * 'csdrLocation'
--
-- * 'csdrStreamingDistribution'
--
-- * 'csdrStatus'
data CreateStreamingDistributionResponse = CreateStreamingDistributionResponse'
    { _csdrETag                  :: !(Maybe Text)
    , _csdrLocation              :: !(Maybe Text)
    , _csdrStreamingDistribution :: !(Maybe StreamingDistribution)
    , _csdrStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateStreamingDistributionResponse' smart constructor.
createStreamingDistributionResponse :: Int -> CreateStreamingDistributionResponse
createStreamingDistributionResponse pStatus =
    CreateStreamingDistributionResponse'
    { _csdrETag = Nothing
    , _csdrLocation = Nothing
    , _csdrStreamingDistribution = Nothing
    , _csdrStatus = pStatus
    }

-- | The current version of the streaming distribution created.
csdrETag :: Lens' CreateStreamingDistributionResponse (Maybe Text)
csdrETag = lens _csdrETag (\ s a -> s{_csdrETag = a});

-- | The fully qualified URI of the new streaming distribution resource just
-- created. For example:
-- https:\/\/cloudfront.amazonaws.com\/2010-11-01\/streaming-distribution\/EGTXBD79H29TRA8.
csdrLocation :: Lens' CreateStreamingDistributionResponse (Maybe Text)
csdrLocation = lens _csdrLocation (\ s a -> s{_csdrLocation = a});

-- | The streaming distribution\'s information.
csdrStreamingDistribution :: Lens' CreateStreamingDistributionResponse (Maybe StreamingDistribution)
csdrStreamingDistribution = lens _csdrStreamingDistribution (\ s a -> s{_csdrStreamingDistribution = a});

-- | FIXME: Undocumented member.
csdrStatus :: Lens' CreateStreamingDistributionResponse Int
csdrStatus = lens _csdrStatus (\ s a -> s{_csdrStatus = a});
