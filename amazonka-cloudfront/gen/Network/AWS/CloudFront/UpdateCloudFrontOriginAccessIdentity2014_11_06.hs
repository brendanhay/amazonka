{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity2014_11_06
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

-- | Update an origin access identity.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/UpdateCloudFrontOriginAccessIdentity2014_11_06.html>
module Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity2014_11_06
    (
    -- * Request
      UpdateCloudFrontOriginAccessIdentity2014_11_06
    -- ** Request constructor
    , updateCloudFrontOriginAccessIdentity2014_11_06
    -- ** Request lenses
    , ucfoaiIfMatch
    , ucfoaiCloudFrontOriginAccessIdentityConfig
    , ucfoaiId

    -- * Response
    , UpdateCloudFrontOriginAccessIdentity2014_11_06Response
    -- ** Response constructor
    , updateCloudFrontOriginAccessIdentity2014_11_06Response
    -- ** Response lenses
    , ucfoairETag
    , ucfoairCloudFrontOriginAccessIdentity
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'updateCloudFrontOriginAccessIdentity2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ucfoaiIfMatch'
--
-- * 'ucfoaiCloudFrontOriginAccessIdentityConfig'
--
-- * 'ucfoaiId'
data UpdateCloudFrontOriginAccessIdentity2014_11_06 = UpdateCloudFrontOriginAccessIdentity2014_11_06'{_ucfoaiIfMatch :: Maybe Text, _ucfoaiCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig, _ucfoaiId :: Text} deriving (Eq, Read, Show)

-- | 'UpdateCloudFrontOriginAccessIdentity2014_11_06' smart constructor.
updateCloudFrontOriginAccessIdentity2014_11_06 :: CloudFrontOriginAccessIdentityConfig -> Text -> UpdateCloudFrontOriginAccessIdentity2014_11_06
updateCloudFrontOriginAccessIdentity2014_11_06 pCloudFrontOriginAccessIdentityConfig pId = UpdateCloudFrontOriginAccessIdentity2014_11_06'{_ucfoaiIfMatch = Nothing, _ucfoaiCloudFrontOriginAccessIdentityConfig = pCloudFrontOriginAccessIdentityConfig, _ucfoaiId = pId};

-- | The value of the ETag header you received when retrieving the
-- identity\'s configuration. For example: E2QWRUHAPOMQZL.
ucfoaiIfMatch :: Lens' UpdateCloudFrontOriginAccessIdentity2014_11_06 (Maybe Text)
ucfoaiIfMatch = lens _ucfoaiIfMatch (\ s a -> s{_ucfoaiIfMatch = a});

-- | The identity\'s configuration information.
ucfoaiCloudFrontOriginAccessIdentityConfig :: Lens' UpdateCloudFrontOriginAccessIdentity2014_11_06 CloudFrontOriginAccessIdentityConfig
ucfoaiCloudFrontOriginAccessIdentityConfig = lens _ucfoaiCloudFrontOriginAccessIdentityConfig (\ s a -> s{_ucfoaiCloudFrontOriginAccessIdentityConfig = a});

-- | The identity\'s id.
ucfoaiId :: Lens' UpdateCloudFrontOriginAccessIdentity2014_11_06 Text
ucfoaiId = lens _ucfoaiId (\ s a -> s{_ucfoaiId = a});

instance AWSRequest
         UpdateCloudFrontOriginAccessIdentity2014_11_06 where
        type Sv
               UpdateCloudFrontOriginAccessIdentity2014_11_06
             = CloudFront
        type Rs
               UpdateCloudFrontOriginAccessIdentity2014_11_06
             =
             UpdateCloudFrontOriginAccessIdentity2014_11_06Response
        request = putXML
        response
          = receiveXML
              (\ s h x ->
                 UpdateCloudFrontOriginAccessIdentity2014_11_06Response'
                   <$>
                   (h .#? "ETag") <*>
                     (x .@? "CloudFrontOriginAccessIdentity"))

instance ToElement
         UpdateCloudFrontOriginAccessIdentity2014_11_06 where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2014-11-06/}CloudFrontOriginAccessIdentityConfig"
              .
              _ucfoaiCloudFrontOriginAccessIdentityConfig

instance ToHeaders
         UpdateCloudFrontOriginAccessIdentity2014_11_06 where
        toHeaders
          UpdateCloudFrontOriginAccessIdentity2014_11_06'{..}
          = mconcat ["If-Match" =# _ucfoaiIfMatch]

instance ToPath
         UpdateCloudFrontOriginAccessIdentity2014_11_06 where
        toPath
          UpdateCloudFrontOriginAccessIdentity2014_11_06'{..}
          = mconcat
              ["/2014-11-06/origin-access-identity/cloudfront/",
               toText _ucfoaiId, "/config"]

instance ToQuery
         UpdateCloudFrontOriginAccessIdentity2014_11_06 where
        toQuery = const mempty

-- | /See:/ 'updateCloudFrontOriginAccessIdentity2014_11_06Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ucfoairETag'
--
-- * 'ucfoairCloudFrontOriginAccessIdentity'
data UpdateCloudFrontOriginAccessIdentity2014_11_06Response = UpdateCloudFrontOriginAccessIdentity2014_11_06Response'{_ucfoairETag :: Maybe Text, _ucfoairCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity} deriving (Eq, Read, Show)

-- | 'UpdateCloudFrontOriginAccessIdentity2014_11_06Response' smart constructor.
updateCloudFrontOriginAccessIdentity2014_11_06Response :: UpdateCloudFrontOriginAccessIdentity2014_11_06Response
updateCloudFrontOriginAccessIdentity2014_11_06Response = UpdateCloudFrontOriginAccessIdentity2014_11_06Response'{_ucfoairETag = Nothing, _ucfoairCloudFrontOriginAccessIdentity = Nothing};

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
ucfoairETag :: Lens' UpdateCloudFrontOriginAccessIdentity2014_11_06Response (Maybe Text)
ucfoairETag = lens _ucfoairETag (\ s a -> s{_ucfoairETag = a});

-- | The origin access identity\'s information.
ucfoairCloudFrontOriginAccessIdentity :: Lens' UpdateCloudFrontOriginAccessIdentity2014_11_06Response (Maybe CloudFrontOriginAccessIdentity)
ucfoairCloudFrontOriginAccessIdentity = lens _ucfoairCloudFrontOriginAccessIdentity (\ s a -> s{_ucfoairCloudFrontOriginAccessIdentity = a});
