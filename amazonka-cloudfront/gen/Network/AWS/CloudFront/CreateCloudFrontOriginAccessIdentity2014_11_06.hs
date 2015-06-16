{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity2014_11_06
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

-- | Create a new origin access identity.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/CreateCloudFrontOriginAccessIdentity2014_11_06.html>
module Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity2014_11_06
    (
    -- * Request
      CreateCloudFrontOriginAccessIdentity2014_11_06
    -- ** Request constructor
    , createCloudFrontOriginAccessIdentity2014_11_06
    -- ** Request lenses
    , ccfoaiCloudFrontOriginAccessIdentityConfig

    -- * Response
    , CreateCloudFrontOriginAccessIdentity2014_11_06Response
    -- ** Response constructor
    , createCloudFrontOriginAccessIdentity2014_11_06Response
    -- ** Response lenses
    , ccfoairETag
    , ccfoairLocation
    , ccfoairCloudFrontOriginAccessIdentity
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'createCloudFrontOriginAccessIdentity2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccfoaiCloudFrontOriginAccessIdentityConfig'
newtype CreateCloudFrontOriginAccessIdentity2014_11_06 = CreateCloudFrontOriginAccessIdentity2014_11_06'{_ccfoaiCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig} deriving (Eq, Read, Show)

-- | 'CreateCloudFrontOriginAccessIdentity2014_11_06' smart constructor.
createCloudFrontOriginAccessIdentity2014_11_06 :: CloudFrontOriginAccessIdentityConfig -> CreateCloudFrontOriginAccessIdentity2014_11_06
createCloudFrontOriginAccessIdentity2014_11_06 pCloudFrontOriginAccessIdentityConfig = CreateCloudFrontOriginAccessIdentity2014_11_06'{_ccfoaiCloudFrontOriginAccessIdentityConfig = pCloudFrontOriginAccessIdentityConfig};

-- | The origin access identity\'s configuration information.
ccfoaiCloudFrontOriginAccessIdentityConfig :: Lens' CreateCloudFrontOriginAccessIdentity2014_11_06 CloudFrontOriginAccessIdentityConfig
ccfoaiCloudFrontOriginAccessIdentityConfig = lens _ccfoaiCloudFrontOriginAccessIdentityConfig (\ s a -> s{_ccfoaiCloudFrontOriginAccessIdentityConfig = a});

instance AWSRequest
         CreateCloudFrontOriginAccessIdentity2014_11_06 where
        type Sv
               CreateCloudFrontOriginAccessIdentity2014_11_06
             = CloudFront
        type Rs
               CreateCloudFrontOriginAccessIdentity2014_11_06
             =
             CreateCloudFrontOriginAccessIdentity2014_11_06Response
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 CreateCloudFrontOriginAccessIdentity2014_11_06Response'
                   <$>
                   (h .#? "ETag") <*> (h .#? "Location") <*>
                     (x .@? "CloudFrontOriginAccessIdentity"))

instance ToElement
         CreateCloudFrontOriginAccessIdentity2014_11_06 where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2014-11-06/}CloudFrontOriginAccessIdentityConfig"
              .
              _ccfoaiCloudFrontOriginAccessIdentityConfig

instance ToHeaders
         CreateCloudFrontOriginAccessIdentity2014_11_06 where
        toHeaders = const mempty

instance ToPath
         CreateCloudFrontOriginAccessIdentity2014_11_06 where
        toPath
          = const
              "/2014-11-06/origin-access-identity/cloudfront"

instance ToQuery
         CreateCloudFrontOriginAccessIdentity2014_11_06 where
        toQuery = const mempty

-- | /See:/ 'createCloudFrontOriginAccessIdentity2014_11_06Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccfoairETag'
--
-- * 'ccfoairLocation'
--
-- * 'ccfoairCloudFrontOriginAccessIdentity'
data CreateCloudFrontOriginAccessIdentity2014_11_06Response = CreateCloudFrontOriginAccessIdentity2014_11_06Response'{_ccfoairETag :: Maybe Text, _ccfoairLocation :: Maybe Text, _ccfoairCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity} deriving (Eq, Read, Show)

-- | 'CreateCloudFrontOriginAccessIdentity2014_11_06Response' smart constructor.
createCloudFrontOriginAccessIdentity2014_11_06Response :: CreateCloudFrontOriginAccessIdentity2014_11_06Response
createCloudFrontOriginAccessIdentity2014_11_06Response = CreateCloudFrontOriginAccessIdentity2014_11_06Response'{_ccfoairETag = Nothing, _ccfoairLocation = Nothing, _ccfoairCloudFrontOriginAccessIdentity = Nothing};

-- | The current version of the origin access identity created.
ccfoairETag :: Lens' CreateCloudFrontOriginAccessIdentity2014_11_06Response (Maybe Text)
ccfoairETag = lens _ccfoairETag (\ s a -> s{_ccfoairETag = a});

-- | The fully qualified URI of the new origin access identity just created.
-- For example:
-- https:\/\/cloudfront.amazonaws.com\/2010-11-01\/origin-access-identity\/cloudfront\/E74FTE3AJFJ256A.
ccfoairLocation :: Lens' CreateCloudFrontOriginAccessIdentity2014_11_06Response (Maybe Text)
ccfoairLocation = lens _ccfoairLocation (\ s a -> s{_ccfoairLocation = a});

-- | The origin access identity\'s information.
ccfoairCloudFrontOriginAccessIdentity :: Lens' CreateCloudFrontOriginAccessIdentity2014_11_06Response (Maybe CloudFrontOriginAccessIdentity)
ccfoairCloudFrontOriginAccessIdentity = lens _ccfoairCloudFrontOriginAccessIdentity (\ s a -> s{_ccfoairCloudFrontOriginAccessIdentity = a});
