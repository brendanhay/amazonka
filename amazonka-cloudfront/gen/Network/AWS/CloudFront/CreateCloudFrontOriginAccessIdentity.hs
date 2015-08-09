{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new origin access identity.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/CreateCloudFrontOriginAccessIdentity.html AWS API Reference> for CreateCloudFrontOriginAccessIdentity.
module Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
    (
    -- * Creating a Request
      createCloudFrontOriginAccessIdentity
    , CreateCloudFrontOriginAccessIdentity
    -- * Request Lenses
    , ccfoaiCloudFrontOriginAccessIdentityConfig

    -- * Destructuring the Response
    , createCloudFrontOriginAccessIdentityResponse
    , CreateCloudFrontOriginAccessIdentityResponse
    -- * Response Lenses
    , ccfoairsETag
    , ccfoairsLocation
    , ccfoairsCloudFrontOriginAccessIdentity
    , ccfoairsStatus
    ) where

import           Network.AWS.CloudFront.Types
import           Network.AWS.CloudFront.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request to create a new origin access identity.
--
-- /See:/ 'createCloudFrontOriginAccessIdentity' smart constructor.
newtype CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentity'
    { _ccfoaiCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateCloudFrontOriginAccessIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccfoaiCloudFrontOriginAccessIdentityConfig'
createCloudFrontOriginAccessIdentity
    :: CloudFrontOriginAccessIdentityConfig -- ^ 'ccfoaiCloudFrontOriginAccessIdentityConfig'
    -> CreateCloudFrontOriginAccessIdentity
createCloudFrontOriginAccessIdentity pCloudFrontOriginAccessIdentityConfig_ =
    CreateCloudFrontOriginAccessIdentity'
    { _ccfoaiCloudFrontOriginAccessIdentityConfig = pCloudFrontOriginAccessIdentityConfig_
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
                     (parseXML x)
                     <*> (pure (fromEnum s)))

instance ToElement
         CreateCloudFrontOriginAccessIdentity where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2015-04-17/}CloudFrontOriginAccessIdentityConfig"
              .
              _ccfoaiCloudFrontOriginAccessIdentityConfig

instance ToHeaders
         CreateCloudFrontOriginAccessIdentity where
        toHeaders = const mempty

instance ToPath CreateCloudFrontOriginAccessIdentity
         where
        toPath
          = const
              "/2015-04-17/origin-access-identity/cloudfront"

instance ToQuery CreateCloudFrontOriginAccessIdentity
         where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'createCloudFrontOriginAccessIdentityResponse' smart constructor.
data CreateCloudFrontOriginAccessIdentityResponse = CreateCloudFrontOriginAccessIdentityResponse'
    { _ccfoairsETag                           :: !(Maybe Text)
    , _ccfoairsLocation                       :: !(Maybe Text)
    , _ccfoairsCloudFrontOriginAccessIdentity :: !(Maybe CloudFrontOriginAccessIdentity)
    , _ccfoairsStatus                         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateCloudFrontOriginAccessIdentityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccfoairsETag'
--
-- * 'ccfoairsLocation'
--
-- * 'ccfoairsCloudFrontOriginAccessIdentity'
--
-- * 'ccfoairsStatus'
createCloudFrontOriginAccessIdentityResponse
    :: Int -- ^ 'ccfoairsStatus'
    -> CreateCloudFrontOriginAccessIdentityResponse
createCloudFrontOriginAccessIdentityResponse pStatus_ =
    CreateCloudFrontOriginAccessIdentityResponse'
    { _ccfoairsETag = Nothing
    , _ccfoairsLocation = Nothing
    , _ccfoairsCloudFrontOriginAccessIdentity = Nothing
    , _ccfoairsStatus = pStatus_
    }

-- | The current version of the origin access identity created.
ccfoairsETag :: Lens' CreateCloudFrontOriginAccessIdentityResponse (Maybe Text)
ccfoairsETag = lens _ccfoairsETag (\ s a -> s{_ccfoairsETag = a});

-- | The fully qualified URI of the new origin access identity just created.
-- For example:
-- https:\/\/cloudfront.amazonaws.com\/2010-11-01\/origin-access-identity\/cloudfront\/E74FTE3AJFJ256A.
ccfoairsLocation :: Lens' CreateCloudFrontOriginAccessIdentityResponse (Maybe Text)
ccfoairsLocation = lens _ccfoairsLocation (\ s a -> s{_ccfoairsLocation = a});

-- | The origin access identity\'s information.
ccfoairsCloudFrontOriginAccessIdentity :: Lens' CreateCloudFrontOriginAccessIdentityResponse (Maybe CloudFrontOriginAccessIdentity)
ccfoairsCloudFrontOriginAccessIdentity = lens _ccfoairsCloudFrontOriginAccessIdentity (\ s a -> s{_ccfoairsCloudFrontOriginAccessIdentity = a});

-- | The response status code.
ccfoairsStatus :: Lens' CreateCloudFrontOriginAccessIdentityResponse Int
ccfoairsStatus = lens _ccfoairsStatus (\ s a -> s{_ccfoairsStatus = a});
