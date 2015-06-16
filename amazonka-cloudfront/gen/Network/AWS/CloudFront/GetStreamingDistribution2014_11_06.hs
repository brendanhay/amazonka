{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.GetStreamingDistribution2014_11_06
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetStreamingDistribution2014_11_06.html>
module Network.AWS.CloudFront.GetStreamingDistribution2014_11_06
    (
    -- * Request
      GetStreamingDistribution2014_11_06
    -- ** Request constructor
    , getStreamingDistribution2014_11_06
    -- ** Request lenses
    , gsdId

    -- * Response
    , GetStreamingDistribution2014_11_06Response
    -- ** Response constructor
    , getStreamingDistribution2014_11_06Response
    -- ** Response lenses
    , gsdrETag
    , gsdrStreamingDistribution
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'getStreamingDistribution2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsdId'
newtype GetStreamingDistribution2014_11_06 = GetStreamingDistribution2014_11_06'{_gsdId :: Text} deriving (Eq, Read, Show)

-- | 'GetStreamingDistribution2014_11_06' smart constructor.
getStreamingDistribution2014_11_06 :: Text -> GetStreamingDistribution2014_11_06
getStreamingDistribution2014_11_06 pId = GetStreamingDistribution2014_11_06'{_gsdId = pId};

-- | The streaming distribution\'s id.
gsdId :: Lens' GetStreamingDistribution2014_11_06 Text
gsdId = lens _gsdId (\ s a -> s{_gsdId = a});

instance AWSRequest
         GetStreamingDistribution2014_11_06 where
        type Sv GetStreamingDistribution2014_11_06 =
             CloudFront
        type Rs GetStreamingDistribution2014_11_06 =
             GetStreamingDistribution2014_11_06Response
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetStreamingDistribution2014_11_06Response' <$>
                   (h .#? "ETag") <*> (x .@? "StreamingDistribution"))

instance ToHeaders GetStreamingDistribution2014_11_06
         where
        toHeaders = const mempty

instance ToPath GetStreamingDistribution2014_11_06
         where
        toPath GetStreamingDistribution2014_11_06'{..}
          = mconcat
              ["/2014-11-06/streaming-distribution/",
               toText _gsdId]

instance ToQuery GetStreamingDistribution2014_11_06
         where
        toQuery = const mempty

-- | /See:/ 'getStreamingDistribution2014_11_06Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsdrETag'
--
-- * 'gsdrStreamingDistribution'
data GetStreamingDistribution2014_11_06Response = GetStreamingDistribution2014_11_06Response'{_gsdrETag :: Maybe Text, _gsdrStreamingDistribution :: Maybe StreamingDistribution} deriving (Eq, Read, Show)

-- | 'GetStreamingDistribution2014_11_06Response' smart constructor.
getStreamingDistribution2014_11_06Response :: GetStreamingDistribution2014_11_06Response
getStreamingDistribution2014_11_06Response = GetStreamingDistribution2014_11_06Response'{_gsdrETag = Nothing, _gsdrStreamingDistribution = Nothing};

-- | The current version of the streaming distribution\'s information. For
-- example: E2QWRUHAPOMQZL.
gsdrETag :: Lens' GetStreamingDistribution2014_11_06Response (Maybe Text)
gsdrETag = lens _gsdrETag (\ s a -> s{_gsdrETag = a});

-- | The streaming distribution\'s information.
gsdrStreamingDistribution :: Lens' GetStreamingDistribution2014_11_06Response (Maybe StreamingDistribution)
gsdrStreamingDistribution = lens _gsdrStreamingDistribution (\ s a -> s{_gsdrStreamingDistribution = a});
