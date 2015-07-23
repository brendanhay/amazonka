{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Update an origin access identity.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/UpdateCloudFrontOriginAccessIdentity.html>
module Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
    (
    -- * Request
      UpdateCloudFrontOriginAccessIdentity
    -- ** Request constructor
    , updateCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , ucfoairqIfMatch
    , ucfoairqCloudFrontOriginAccessIdentityConfig
    , ucfoairqId

    -- * Response
    , UpdateCloudFrontOriginAccessIdentityResponse
    -- ** Response constructor
    , updateCloudFrontOriginAccessIdentityResponse
    -- ** Response lenses
    , ucfoairsETag
    , ucfoairsCloudFrontOriginAccessIdentity
    , ucfoairsStatus
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
-- * 'ucfoairqIfMatch'
--
-- * 'ucfoairqCloudFrontOriginAccessIdentityConfig'
--
-- * 'ucfoairqId'
data UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentity'
    { _ucfoairqIfMatch                              :: !(Maybe Text)
    , _ucfoairqCloudFrontOriginAccessIdentityConfig :: !CloudFrontOriginAccessIdentityConfig
    , _ucfoairqId                                   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateCloudFrontOriginAccessIdentity' smart constructor.
updateCloudFrontOriginAccessIdentity :: CloudFrontOriginAccessIdentityConfig -> Text -> UpdateCloudFrontOriginAccessIdentity
updateCloudFrontOriginAccessIdentity pCloudFrontOriginAccessIdentityConfig_ pId_ =
    UpdateCloudFrontOriginAccessIdentity'
    { _ucfoairqIfMatch = Nothing
    , _ucfoairqCloudFrontOriginAccessIdentityConfig = pCloudFrontOriginAccessIdentityConfig_
    , _ucfoairqId = pId_
    }

-- | The value of the ETag header you received when retrieving the
-- identity\'s configuration. For example: E2QWRUHAPOMQZL.
ucfoairqIfMatch :: Lens' UpdateCloudFrontOriginAccessIdentity (Maybe Text)
ucfoairqIfMatch = lens _ucfoairqIfMatch (\ s a -> s{_ucfoairqIfMatch = a});

-- | The identity\'s configuration information.
ucfoairqCloudFrontOriginAccessIdentityConfig :: Lens' UpdateCloudFrontOriginAccessIdentity CloudFrontOriginAccessIdentityConfig
ucfoairqCloudFrontOriginAccessIdentityConfig = lens _ucfoairqCloudFrontOriginAccessIdentityConfig (\ s a -> s{_ucfoairqCloudFrontOriginAccessIdentityConfig = a});

-- | The identity\'s id.
ucfoairqId :: Lens' UpdateCloudFrontOriginAccessIdentity Text
ucfoairqId = lens _ucfoairqId (\ s a -> s{_ucfoairqId = a});

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
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (pure (fromEnum s)))

instance ToElement
         UpdateCloudFrontOriginAccessIdentity where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2015-04-17/}CloudFrontOriginAccessIdentityConfig"
              .
              _ucfoairqCloudFrontOriginAccessIdentityConfig

instance ToHeaders
         UpdateCloudFrontOriginAccessIdentity where
        toHeaders UpdateCloudFrontOriginAccessIdentity'{..}
          = mconcat ["If-Match" =# _ucfoairqIfMatch]

instance ToPath UpdateCloudFrontOriginAccessIdentity
         where
        toPath UpdateCloudFrontOriginAccessIdentity'{..}
          = mconcat
              ["/2015-04-17/origin-access-identity/cloudfront/",
               toText _ucfoairqId, "/config"]

instance ToQuery UpdateCloudFrontOriginAccessIdentity
         where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'updateCloudFrontOriginAccessIdentityResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ucfoairsETag'
--
-- * 'ucfoairsCloudFrontOriginAccessIdentity'
--
-- * 'ucfoairsStatus'
data UpdateCloudFrontOriginAccessIdentityResponse = UpdateCloudFrontOriginAccessIdentityResponse'
    { _ucfoairsETag                           :: !(Maybe Text)
    , _ucfoairsCloudFrontOriginAccessIdentity :: !(Maybe CloudFrontOriginAccessIdentity)
    , _ucfoairsStatus                         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateCloudFrontOriginAccessIdentityResponse' smart constructor.
updateCloudFrontOriginAccessIdentityResponse :: Int -> UpdateCloudFrontOriginAccessIdentityResponse
updateCloudFrontOriginAccessIdentityResponse pStatus_ =
    UpdateCloudFrontOriginAccessIdentityResponse'
    { _ucfoairsETag = Nothing
    , _ucfoairsCloudFrontOriginAccessIdentity = Nothing
    , _ucfoairsStatus = pStatus_
    }

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
ucfoairsETag :: Lens' UpdateCloudFrontOriginAccessIdentityResponse (Maybe Text)
ucfoairsETag = lens _ucfoairsETag (\ s a -> s{_ucfoairsETag = a});

-- | The origin access identity\'s information.
ucfoairsCloudFrontOriginAccessIdentity :: Lens' UpdateCloudFrontOriginAccessIdentityResponse (Maybe CloudFrontOriginAccessIdentity)
ucfoairsCloudFrontOriginAccessIdentity = lens _ucfoairsCloudFrontOriginAccessIdentity (\ s a -> s{_ucfoairsCloudFrontOriginAccessIdentity = a});

-- | FIXME: Undocumented member.
ucfoairsStatus :: Lens' UpdateCloudFrontOriginAccessIdentityResponse Int
ucfoairsStatus = lens _ucfoairsStatus (\ s a -> s{_ucfoairsStatus = a});
