{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
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
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/CreateCloudFrontOriginAccessIdentity.html>
module Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
    (
    -- * Request
      CreateCloudFrontOriginAccessIdentity
    -- ** Request constructor
    , createCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , ccfoaiCloudFrontOriginAccessIdentityConfig

    -- * Response
    , CreateCloudFrontOriginAccessIdentityResponse
    -- ** Response constructor
    , createCloudFrontOriginAccessIdentityResponse
    -- ** Response lenses
    , ccfoairETag
    , ccfoairLocation
    , ccfoairCloudFrontOriginAccessIdentity
    , ccfoairStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to create a new origin access identity.
--
-- /See:/ 'createCloudFrontOriginAccessIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccfoaiCloudFrontOriginAccessIdentityConfig'
newtype CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentity'
    { _ccfoaiCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
    } deriving (Eq,Read,Show)

-- | 'CreateCloudFrontOriginAccessIdentity' smart constructor.
createCloudFrontOriginAccessIdentity :: CloudFrontOriginAccessIdentityConfig -> CreateCloudFrontOriginAccessIdentity
createCloudFrontOriginAccessIdentity pCloudFrontOriginAccessIdentityConfig =
    CreateCloudFrontOriginAccessIdentity'
    { _ccfoaiCloudFrontOriginAccessIdentityConfig = pCloudFrontOriginAccessIdentityConfig
    }

-- | The origin access identity\'s configuration information.
ccfoaiCloudFrontOriginAccessIdentityConfig :: Lens' CreateCloudFrontOriginAccessIdentity CloudFrontOriginAccessIdentityConfig
ccfoaiCloudFrontOriginAccessIdentityConfig = lens _ccfoaiCloudFrontOriginAccessIdentityConfig (\ s a -> s{_ccfoaiCloudFrontOriginAccessIdentityConfig = a});

instance AWSRequest
         CreateCloudFrontOriginAccessIdentity where
        type Sv CreateCloudFrontOriginAccessIdentity =
             CloudFront
        type Rs CreateCloudFrontOriginAccessIdentity =
             CreateCloudFrontOriginAccessIdentityResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 CreateCloudFrontOriginAccessIdentityResponse' <$>
                   (h .#? "ETag") <*> (h .#? "Location") <*>
                     (x .@? "CloudFrontOriginAccessIdentity")
                     <*> (pure (fromEnum s)))

instance ToElement
         CreateCloudFrontOriginAccessIdentity where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2014-11-06/}CloudFrontOriginAccessIdentityConfig"
              .
              _ccfoaiCloudFrontOriginAccessIdentityConfig

instance ToHeaders
         CreateCloudFrontOriginAccessIdentity where
        toHeaders = const mempty

instance ToPath CreateCloudFrontOriginAccessIdentity
         where
        toPath
          = const
              "/2014-11-06/origin-access-identity/cloudfront"

instance ToQuery CreateCloudFrontOriginAccessIdentity
         where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'createCloudFrontOriginAccessIdentityResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccfoairETag'
--
-- * 'ccfoairLocation'
--
-- * 'ccfoairCloudFrontOriginAccessIdentity'
--
-- * 'ccfoairStatus'
data CreateCloudFrontOriginAccessIdentityResponse = CreateCloudFrontOriginAccessIdentityResponse'
    { _ccfoairETag                           :: Maybe Text
    , _ccfoairLocation                       :: Maybe Text
    , _ccfoairCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
    , _ccfoairStatus                         :: !Int
    } deriving (Eq,Read,Show)

-- | 'CreateCloudFrontOriginAccessIdentityResponse' smart constructor.
createCloudFrontOriginAccessIdentityResponse :: Int -> CreateCloudFrontOriginAccessIdentityResponse
createCloudFrontOriginAccessIdentityResponse pStatus =
    CreateCloudFrontOriginAccessIdentityResponse'
    { _ccfoairETag = Nothing
    , _ccfoairLocation = Nothing
    , _ccfoairCloudFrontOriginAccessIdentity = Nothing
    , _ccfoairStatus = pStatus
    }

-- | The current version of the origin access identity created.
ccfoairETag :: Lens' CreateCloudFrontOriginAccessIdentityResponse (Maybe Text)
ccfoairETag = lens _ccfoairETag (\ s a -> s{_ccfoairETag = a});

-- | The fully qualified URI of the new origin access identity just created.
-- For example:
-- https:\/\/cloudfront.amazonaws.com\/2010-11-01\/origin-access-identity\/cloudfront\/E74FTE3AJFJ256A.
ccfoairLocation :: Lens' CreateCloudFrontOriginAccessIdentityResponse (Maybe Text)
ccfoairLocation = lens _ccfoairLocation (\ s a -> s{_ccfoairLocation = a});

-- | The origin access identity\'s information.
ccfoairCloudFrontOriginAccessIdentity :: Lens' CreateCloudFrontOriginAccessIdentityResponse (Maybe CloudFrontOriginAccessIdentity)
ccfoairCloudFrontOriginAccessIdentity = lens _ccfoairCloudFrontOriginAccessIdentity (\ s a -> s{_ccfoairCloudFrontOriginAccessIdentity = a});

-- | FIXME: Undocumented member.
ccfoairStatus :: Lens' CreateCloudFrontOriginAccessIdentityResponse Int
ccfoairStatus = lens _ccfoairStatus (\ s a -> s{_ccfoairStatus = a});
