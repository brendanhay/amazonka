{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.UpdateDistribution2014_11_06
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

-- | Update a distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/UpdateDistribution2014_11_06.html>
module Network.AWS.CloudFront.UpdateDistribution2014_11_06
    (
    -- * Request
      UpdateDistribution2014_11_06
    -- ** Request constructor
    , updateDistribution2014_11_06
    -- ** Request lenses
    , udIfMatch
    , udDistributionConfig
    , udId

    -- * Response
    , UpdateDistribution2014_11_06Response
    -- ** Response constructor
    , updateDistribution2014_11_06Response
    -- ** Response lenses
    , udrETag
    , udrDistribution
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'updateDistribution2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udIfMatch'
--
-- * 'udDistributionConfig'
--
-- * 'udId'
data UpdateDistribution2014_11_06 = UpdateDistribution2014_11_06'{_udIfMatch :: Maybe Text, _udDistributionConfig :: DistributionConfig, _udId :: Text} deriving (Eq, Read, Show)

-- | 'UpdateDistribution2014_11_06' smart constructor.
updateDistribution2014_11_06 :: DistributionConfig -> Text -> UpdateDistribution2014_11_06
updateDistribution2014_11_06 pDistributionConfig pId = UpdateDistribution2014_11_06'{_udIfMatch = Nothing, _udDistributionConfig = pDistributionConfig, _udId = pId};

-- | The value of the ETag header you received when retrieving the
-- distribution\'s configuration. For example: E2QWRUHAPOMQZL.
udIfMatch :: Lens' UpdateDistribution2014_11_06 (Maybe Text)
udIfMatch = lens _udIfMatch (\ s a -> s{_udIfMatch = a});

-- | The distribution\'s configuration information.
udDistributionConfig :: Lens' UpdateDistribution2014_11_06 DistributionConfig
udDistributionConfig = lens _udDistributionConfig (\ s a -> s{_udDistributionConfig = a});

-- | The distribution\'s id.
udId :: Lens' UpdateDistribution2014_11_06 Text
udId = lens _udId (\ s a -> s{_udId = a});

instance AWSRequest UpdateDistribution2014_11_06
         where
        type Sv UpdateDistribution2014_11_06 = CloudFront
        type Rs UpdateDistribution2014_11_06 =
             UpdateDistribution2014_11_06Response
        request = putXML
        response
          = receiveXML
              (\ s h x ->
                 UpdateDistribution2014_11_06Response' <$>
                   (h .#? "ETag") <*> (x .@? "Distribution"))

instance ToElement UpdateDistribution2014_11_06 where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2014-11-06/}DistributionConfig"
              .
              _udDistributionConfig

instance ToHeaders UpdateDistribution2014_11_06 where
        toHeaders UpdateDistribution2014_11_06'{..}
          = mconcat ["If-Match" =# _udIfMatch]

instance ToPath UpdateDistribution2014_11_06 where
        toPath UpdateDistribution2014_11_06'{..}
          = mconcat
              ["/2014-11-06/distribution/", toText _udId,
               "/config"]

instance ToQuery UpdateDistribution2014_11_06 where
        toQuery = const mempty

-- | /See:/ 'updateDistribution2014_11_06Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udrETag'
--
-- * 'udrDistribution'
data UpdateDistribution2014_11_06Response = UpdateDistribution2014_11_06Response'{_udrETag :: Maybe Text, _udrDistribution :: Maybe Distribution} deriving (Eq, Read, Show)

-- | 'UpdateDistribution2014_11_06Response' smart constructor.
updateDistribution2014_11_06Response :: UpdateDistribution2014_11_06Response
updateDistribution2014_11_06Response = UpdateDistribution2014_11_06Response'{_udrETag = Nothing, _udrDistribution = Nothing};

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
udrETag :: Lens' UpdateDistribution2014_11_06Response (Maybe Text)
udrETag = lens _udrETag (\ s a -> s{_udrETag = a});

-- | The distribution\'s information.
udrDistribution :: Lens' UpdateDistribution2014_11_06Response (Maybe Distribution)
udrDistribution = lens _udrDistribution (\ s a -> s{_udrDistribution = a});
