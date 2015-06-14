{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.CreateDistribution2014_11_06
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
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/CreateDistribution2014_11_06.html>
module Network.AWS.CloudFront.CreateDistribution2014_11_06
    (
    -- * Request
      CreateDistribution2014_11_06
    -- ** Request constructor
    , createDistribution2014_11_06
    -- ** Request lenses
    , cdDistributionConfig

    -- * Response
    , CreateDistribution2014_11_06Response
    -- ** Response constructor
    , createDistribution2014_11_06Response
    -- ** Response lenses
    , cdrETag
    , cdrDistribution
    , cdrLocation
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'createDistribution2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdDistributionConfig'
newtype CreateDistribution2014_11_06 = CreateDistribution2014_11_06'{_cdDistributionConfig :: DistributionConfig} deriving (Eq, Read, Show)

-- | 'CreateDistribution2014_11_06' smart constructor.
createDistribution2014_11_06 :: DistributionConfig -> CreateDistribution2014_11_06
createDistribution2014_11_06 pDistributionConfig = CreateDistribution2014_11_06'{_cdDistributionConfig = pDistributionConfig};

-- | The distribution\'s configuration information.
cdDistributionConfig :: Lens' CreateDistribution2014_11_06 DistributionConfig
cdDistributionConfig = lens _cdDistributionConfig (\ s a -> s{_cdDistributionConfig = a});

instance AWSRequest CreateDistribution2014_11_06
         where
        type Sv CreateDistribution2014_11_06 = CloudFront
        type Rs CreateDistribution2014_11_06 =
             CreateDistribution2014_11_06Response
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 CreateDistribution2014_11_06Response' <$>
                   h .#? "ETag" <*> x .@? "Distribution" <*>
                     h .#? "Location")

instance ToElement CreateDistribution2014_11_06 where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2014-11-06/}DistributionConfig"
              .
              _cdDistributionConfig

instance ToHeaders CreateDistribution2014_11_06 where
        toHeaders = const mempty

instance ToPath CreateDistribution2014_11_06 where
        toPath = const "/2014-11-06/distribution"

instance ToQuery CreateDistribution2014_11_06 where
        toQuery = const mempty

-- | /See:/ 'createDistribution2014_11_06Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdrETag'
--
-- * 'cdrDistribution'
--
-- * 'cdrLocation'
data CreateDistribution2014_11_06Response = CreateDistribution2014_11_06Response'{_cdrETag :: Maybe Text, _cdrDistribution :: Maybe Distribution, _cdrLocation :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CreateDistribution2014_11_06Response' smart constructor.
createDistribution2014_11_06Response :: CreateDistribution2014_11_06Response
createDistribution2014_11_06Response = CreateDistribution2014_11_06Response'{_cdrETag = Nothing, _cdrDistribution = Nothing, _cdrLocation = Nothing};

-- | The current version of the distribution created.
cdrETag :: Lens' CreateDistribution2014_11_06Response (Maybe Text)
cdrETag = lens _cdrETag (\ s a -> s{_cdrETag = a});

-- | The distribution\'s information.
cdrDistribution :: Lens' CreateDistribution2014_11_06Response (Maybe Distribution)
cdrDistribution = lens _cdrDistribution (\ s a -> s{_cdrDistribution = a});

-- | The fully qualified URI of the new distribution resource just created.
-- For example:
-- https:\/\/cloudfront.amazonaws.com\/2010-11-01\/distribution\/EDFDVBD632BHDS5.
cdrLocation :: Lens' CreateDistribution2014_11_06Response (Maybe Text)
cdrLocation = lens _cdrLocation (\ s a -> s{_cdrLocation = a});
