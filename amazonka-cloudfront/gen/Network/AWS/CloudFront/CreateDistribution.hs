{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.CreateDistribution
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

-- | Create a new distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/CreateDistribution.html>
module Network.AWS.CloudFront.CreateDistribution
    (
    -- * Request
      CreateDistribution
    -- ** Request constructor
    , createDistribution
    -- ** Request lenses
    , cdDistributionConfig

    -- * Response
    , CreateDistributionResponse
    -- ** Response constructor
    , createDistributionResponse
    -- ** Response lenses
    , cdrETag
    , cdrDistribution
    , cdrLocation
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDistribution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdDistributionConfig'
newtype CreateDistribution = CreateDistribution'{_cdDistributionConfig :: DistributionConfig} deriving (Eq, Read, Show)

-- | 'CreateDistribution' smart constructor.
createDistribution :: DistributionConfig -> CreateDistribution
createDistribution pDistributionConfig = CreateDistribution'{_cdDistributionConfig = pDistributionConfig};

-- | The distribution\'s configuration information.
cdDistributionConfig :: Lens' CreateDistribution DistributionConfig
cdDistributionConfig = lens _cdDistributionConfig (\ s a -> s{_cdDistributionConfig = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest CreateDistribution where
        type Sv CreateDistribution = CloudFront
        type Rs CreateDistribution =
             CreateDistributionResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 CreateDistributionResponse' <$>
                   (h .#? "ETag") <*> (x .@? "Distribution") <*>
                     (h .#? "Location"))

instance ToElement CreateDistribution where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2014-11-06/}DistributionConfig"
              .
              _cdDistributionConfig

instance ToHeaders CreateDistribution where
        toHeaders = const mempty

instance ToPath CreateDistribution where
        toPath = const "/2014-11-06/distribution"

instance ToQuery CreateDistribution where
        toQuery = const mempty

-- | /See:/ 'createDistributionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdrETag'
--
-- * 'cdrDistribution'
--
-- * 'cdrLocation'
data CreateDistributionResponse = CreateDistributionResponse'{_cdrETag :: Maybe Text, _cdrDistribution :: Maybe Distribution, _cdrLocation :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CreateDistributionResponse' smart constructor.
createDistributionResponse :: CreateDistributionResponse
createDistributionResponse = CreateDistributionResponse'{_cdrETag = Nothing, _cdrDistribution = Nothing, _cdrLocation = Nothing};

-- | The current version of the distribution created.
cdrETag :: Lens' CreateDistributionResponse (Maybe Text)
cdrETag = lens _cdrETag (\ s a -> s{_cdrETag = a});

-- | The distribution\'s information.
cdrDistribution :: Lens' CreateDistributionResponse (Maybe Distribution)
cdrDistribution = lens _cdrDistribution (\ s a -> s{_cdrDistribution = a});

-- | The fully qualified URI of the new distribution resource just created.
-- For example:
-- https:\/\/cloudfront.amazonaws.com\/2010-11-01\/distribution\/EDFDVBD632BHDS5.
cdrLocation :: Lens' CreateDistributionResponse (Maybe Text)
cdrLocation = lens _cdrLocation (\ s a -> s{_cdrLocation = a});
