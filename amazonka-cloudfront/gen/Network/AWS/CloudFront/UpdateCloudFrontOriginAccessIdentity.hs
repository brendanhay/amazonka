{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
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
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/UpdateCloudFrontOriginAccessIdentity.html>
module Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
    (
    -- * Request
      UpdateCloudFrontOriginAccessIdentity
    -- ** Request constructor
    , updateCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , ucfoaiIfMatch
    , ucfoaiCloudFrontOriginAccessIdentityConfig
    , ucfoaiId

    -- * Response
    , UpdateCloudFrontOriginAccessIdentityResponse
    -- ** Response constructor
    , updateCloudFrontOriginAccessIdentityResponse
    -- ** Response lenses
    , ucfoairETag
    , ucfoairCloudFrontOriginAccessIdentity
    , ucfoairStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to update an origin access identity.
--
-- /See:/ 'updateCloudFrontOriginAccessIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ucfoaiIfMatch'
--
-- * 'ucfoaiCloudFrontOriginAccessIdentityConfig'
--
-- * 'ucfoaiId'
data UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentity'
    { _ucfoaiIfMatch                              :: Maybe Text
    , _ucfoaiCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
    , _ucfoaiId                                   :: Text
    } deriving (Eq,Read,Show)

-- | 'UpdateCloudFrontOriginAccessIdentity' smart constructor.
updateCloudFrontOriginAccessIdentity :: CloudFrontOriginAccessIdentityConfig -> Text -> UpdateCloudFrontOriginAccessIdentity
updateCloudFrontOriginAccessIdentity pCloudFrontOriginAccessIdentityConfig pId =
    UpdateCloudFrontOriginAccessIdentity'
    { _ucfoaiIfMatch = Nothing
    , _ucfoaiCloudFrontOriginAccessIdentityConfig = pCloudFrontOriginAccessIdentityConfig
    , _ucfoaiId = pId
    }

-- | The value of the ETag header you received when retrieving the
-- identity\'s configuration. For example: E2QWRUHAPOMQZL.
ucfoaiIfMatch :: Lens' UpdateCloudFrontOriginAccessIdentity (Maybe Text)
ucfoaiIfMatch = lens _ucfoaiIfMatch (\ s a -> s{_ucfoaiIfMatch = a});

-- | The identity\'s configuration information.
ucfoaiCloudFrontOriginAccessIdentityConfig :: Lens' UpdateCloudFrontOriginAccessIdentity CloudFrontOriginAccessIdentityConfig
ucfoaiCloudFrontOriginAccessIdentityConfig = lens _ucfoaiCloudFrontOriginAccessIdentityConfig (\ s a -> s{_ucfoaiCloudFrontOriginAccessIdentityConfig = a});

-- | The identity\'s id.
ucfoaiId :: Lens' UpdateCloudFrontOriginAccessIdentity Text
ucfoaiId = lens _ucfoaiId (\ s a -> s{_ucfoaiId = a});

instance AWSRequest
         UpdateCloudFrontOriginAccessIdentity where
        type Sv UpdateCloudFrontOriginAccessIdentity =
             CloudFront
        type Rs UpdateCloudFrontOriginAccessIdentity =
             UpdateCloudFrontOriginAccessIdentityResponse
        request = putXML
        response
          = receiveXML
              (\ s h x ->
                 UpdateCloudFrontOriginAccessIdentityResponse' <$>
                   (h .#? "ETag") <*>
                     (x .@? "CloudFrontOriginAccessIdentity")
                     <*> (pure (fromEnum s)))

instance ToElement
         UpdateCloudFrontOriginAccessIdentity where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2015-04-17/}CloudFrontOriginAccessIdentityConfig"
              .
              _ucfoaiCloudFrontOriginAccessIdentityConfig

instance ToHeaders
         UpdateCloudFrontOriginAccessIdentity where
        toHeaders UpdateCloudFrontOriginAccessIdentity'{..}
          = mconcat ["If-Match" =# _ucfoaiIfMatch]

instance ToPath UpdateCloudFrontOriginAccessIdentity
         where
        toPath UpdateCloudFrontOriginAccessIdentity'{..}
          = mconcat
              ["/2015-04-17/origin-access-identity/cloudfront/",
               toText _ucfoaiId, "/config"]

instance ToQuery UpdateCloudFrontOriginAccessIdentity
         where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'updateCloudFrontOriginAccessIdentityResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ucfoairETag'
--
-- * 'ucfoairCloudFrontOriginAccessIdentity'
--
-- * 'ucfoairStatus'
data UpdateCloudFrontOriginAccessIdentityResponse = UpdateCloudFrontOriginAccessIdentityResponse'
    { _ucfoairETag                           :: Maybe Text
    , _ucfoairCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
    , _ucfoairStatus                         :: !Int
    } deriving (Eq,Read,Show)

-- | 'UpdateCloudFrontOriginAccessIdentityResponse' smart constructor.
updateCloudFrontOriginAccessIdentityResponse :: Int -> UpdateCloudFrontOriginAccessIdentityResponse
updateCloudFrontOriginAccessIdentityResponse pStatus =
    UpdateCloudFrontOriginAccessIdentityResponse'
    { _ucfoairETag = Nothing
    , _ucfoairCloudFrontOriginAccessIdentity = Nothing
    , _ucfoairStatus = pStatus
    }

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
ucfoairETag :: Lens' UpdateCloudFrontOriginAccessIdentityResponse (Maybe Text)
ucfoairETag = lens _ucfoairETag (\ s a -> s{_ucfoairETag = a});

-- | The origin access identity\'s information.
ucfoairCloudFrontOriginAccessIdentity :: Lens' UpdateCloudFrontOriginAccessIdentityResponse (Maybe CloudFrontOriginAccessIdentity)
ucfoairCloudFrontOriginAccessIdentity = lens _ucfoairCloudFrontOriginAccessIdentity (\ s a -> s{_ucfoairCloudFrontOriginAccessIdentity = a});

-- | FIXME: Undocumented member.
ucfoairStatus :: Lens' UpdateCloudFrontOriginAccessIdentityResponse Int
ucfoairStatus = lens _ucfoairStatus (\ s a -> s{_ucfoairStatus = a});
