{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudFront.GetStreamingDistribution
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

-- | Get the information about a streaming distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetStreamingDistribution.html>
module Network.AWS.CloudFront.GetStreamingDistribution
    (
    -- * Request
      GetStreamingDistribution
    -- ** Request constructor
    , getStreamingDistribution
    -- ** Request lenses
    , gsdId

    -- * Response
    , GetStreamingDistributionResponse
    -- ** Response constructor
    , getStreamingDistributionResponse
    -- ** Response lenses
    , gsdrETag
    , gsdrStreamingDistribution
    , gsdrStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to get a streaming distribution\'s information.
--
-- /See:/ 'getStreamingDistribution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsdId'
newtype GetStreamingDistribution = GetStreamingDistribution'
    { _gsdId :: Text
    } deriving (Eq,Read,Show)

-- | 'GetStreamingDistribution' smart constructor.
getStreamingDistribution :: Text -> GetStreamingDistribution
getStreamingDistribution pId =
    GetStreamingDistribution'
    { _gsdId = pId
    }

-- | The streaming distribution\'s id.
gsdId :: Lens' GetStreamingDistribution Text
gsdId = lens _gsdId (\ s a -> s{_gsdId = a});

instance AWSRequest GetStreamingDistribution where
        type Sv GetStreamingDistribution = CloudFront
        type Rs GetStreamingDistribution =
             GetStreamingDistributionResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetStreamingDistributionResponse' <$>
                   (h .#? "ETag") <*> (x .@? "StreamingDistribution")
                     <*> (pure (fromEnum s)))

instance ToHeaders GetStreamingDistribution where
        toHeaders = const mempty

instance ToPath GetStreamingDistribution where
        toPath GetStreamingDistribution'{..}
          = mconcat
              ["/2015-04-17/streaming-distribution/",
               toText _gsdId]

instance ToQuery GetStreamingDistribution where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'getStreamingDistributionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsdrETag'
--
-- * 'gsdrStreamingDistribution'
--
-- * 'gsdrStatus'
data GetStreamingDistributionResponse = GetStreamingDistributionResponse'
    { _gsdrETag                  :: !(Maybe Text)
    , _gsdrStreamingDistribution :: !(Maybe StreamingDistribution)
    , _gsdrStatus                :: !Int
    } deriving (Eq,Read,Show)

-- | 'GetStreamingDistributionResponse' smart constructor.
getStreamingDistributionResponse :: Int -> GetStreamingDistributionResponse
getStreamingDistributionResponse pStatus =
    GetStreamingDistributionResponse'
    { _gsdrETag = Nothing
    , _gsdrStreamingDistribution = Nothing
    , _gsdrStatus = pStatus
    }

-- | The current version of the streaming distribution\'s information. For
-- example: E2QWRUHAPOMQZL.
gsdrETag :: Lens' GetStreamingDistributionResponse (Maybe Text)
gsdrETag = lens _gsdrETag (\ s a -> s{_gsdrETag = a});

-- | The streaming distribution\'s information.
gsdrStreamingDistribution :: Lens' GetStreamingDistributionResponse (Maybe StreamingDistribution)
gsdrStreamingDistribution = lens _gsdrStreamingDistribution (\ s a -> s{_gsdrStreamingDistribution = a});

-- | FIXME: Undocumented member.
gsdrStatus :: Lens' GetStreamingDistributionResponse Int
gsdrStatus = lens _gsdrStatus (\ s a -> s{_gsdrStatus = a});
