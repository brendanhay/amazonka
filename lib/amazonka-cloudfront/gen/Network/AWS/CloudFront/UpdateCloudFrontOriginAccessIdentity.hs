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
-- Module      : Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an origin access identity.
--
--
module Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
    (
    -- * Creating a Request
      updateCloudFrontOriginAccessIdentity
    , UpdateCloudFrontOriginAccessIdentity
    -- * Request Lenses
    , ucfoaiIfMatch
    , ucfoaiCloudFrontOriginAccessIdentityConfig
    , ucfoaiId

    -- * Destructuring the Response
    , updateCloudFrontOriginAccessIdentityResponse
    , UpdateCloudFrontOriginAccessIdentityResponse
    -- * Response Lenses
    , ucfoairsETag
    , ucfoairsCloudFrontOriginAccessIdentity
    , ucfoairsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to update an origin access identity.
--
--
--
-- /See:/ 'updateCloudFrontOriginAccessIdentity' smart constructor.
data UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentity'
  { _ucfoaiIfMatch :: !(Maybe Text)
  , _ucfoaiCloudFrontOriginAccessIdentityConfig :: !CloudFrontOriginAccessIdentityConfig
  , _ucfoaiId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCloudFrontOriginAccessIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucfoaiIfMatch' - The value of the @ETag@ header that you received when retrieving the identity's configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'ucfoaiCloudFrontOriginAccessIdentityConfig' - The identity's configuration information.
--
-- * 'ucfoaiId' - The identity's id.
updateCloudFrontOriginAccessIdentity
    :: CloudFrontOriginAccessIdentityConfig -- ^ 'ucfoaiCloudFrontOriginAccessIdentityConfig'
    -> Text -- ^ 'ucfoaiId'
    -> UpdateCloudFrontOriginAccessIdentity
updateCloudFrontOriginAccessIdentity pCloudFrontOriginAccessIdentityConfig_ pId_ =
  UpdateCloudFrontOriginAccessIdentity'
    { _ucfoaiIfMatch = Nothing
    , _ucfoaiCloudFrontOriginAccessIdentityConfig =
        pCloudFrontOriginAccessIdentityConfig_
    , _ucfoaiId = pId_
    }


-- | The value of the @ETag@ header that you received when retrieving the identity's configuration. For example: @E2QWRUHAPOMQZL@ .
ucfoaiIfMatch :: Lens' UpdateCloudFrontOriginAccessIdentity (Maybe Text)
ucfoaiIfMatch = lens _ucfoaiIfMatch (\ s a -> s{_ucfoaiIfMatch = a})

-- | The identity's configuration information.
ucfoaiCloudFrontOriginAccessIdentityConfig :: Lens' UpdateCloudFrontOriginAccessIdentity CloudFrontOriginAccessIdentityConfig
ucfoaiCloudFrontOriginAccessIdentityConfig = lens _ucfoaiCloudFrontOriginAccessIdentityConfig (\ s a -> s{_ucfoaiCloudFrontOriginAccessIdentityConfig = a})

-- | The identity's id.
ucfoaiId :: Lens' UpdateCloudFrontOriginAccessIdentity Text
ucfoaiId = lens _ucfoaiId (\ s a -> s{_ucfoaiId = a})

instance AWSRequest
           UpdateCloudFrontOriginAccessIdentity
         where
        type Rs UpdateCloudFrontOriginAccessIdentity =
             UpdateCloudFrontOriginAccessIdentityResponse
        request = putXML cloudFront
        response
          = receiveXML
              (\ s h x ->
                 UpdateCloudFrontOriginAccessIdentityResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (pure (fromEnum s)))

instance Hashable
           UpdateCloudFrontOriginAccessIdentity
         where

instance NFData UpdateCloudFrontOriginAccessIdentity
         where

instance ToElement
           UpdateCloudFrontOriginAccessIdentity
         where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2017-10-30/}CloudFrontOriginAccessIdentityConfig"
              .
              _ucfoaiCloudFrontOriginAccessIdentityConfig

instance ToHeaders
           UpdateCloudFrontOriginAccessIdentity
         where
        toHeaders UpdateCloudFrontOriginAccessIdentity'{..}
          = mconcat ["If-Match" =# _ucfoaiIfMatch]

instance ToPath UpdateCloudFrontOriginAccessIdentity
         where
        toPath UpdateCloudFrontOriginAccessIdentity'{..}
          = mconcat
              ["/2017-10-30/origin-access-identity/cloudfront/",
               toBS _ucfoaiId, "/config"]

instance ToQuery UpdateCloudFrontOriginAccessIdentity
         where
        toQuery = const mempty

-- | The returned result of the corresponding request.
--
--
--
-- /See:/ 'updateCloudFrontOriginAccessIdentityResponse' smart constructor.
data UpdateCloudFrontOriginAccessIdentityResponse = UpdateCloudFrontOriginAccessIdentityResponse'
  { _ucfoairsETag :: !(Maybe Text)
  , _ucfoairsCloudFrontOriginAccessIdentity :: !(Maybe CloudFrontOriginAccessIdentity)
  , _ucfoairsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCloudFrontOriginAccessIdentityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucfoairsETag' - The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'ucfoairsCloudFrontOriginAccessIdentity' - The origin access identity's information.
--
-- * 'ucfoairsResponseStatus' - -- | The response status code.
updateCloudFrontOriginAccessIdentityResponse
    :: Int -- ^ 'ucfoairsResponseStatus'
    -> UpdateCloudFrontOriginAccessIdentityResponse
updateCloudFrontOriginAccessIdentityResponse pResponseStatus_ =
  UpdateCloudFrontOriginAccessIdentityResponse'
    { _ucfoairsETag = Nothing
    , _ucfoairsCloudFrontOriginAccessIdentity = Nothing
    , _ucfoairsResponseStatus = pResponseStatus_
    }


-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
ucfoairsETag :: Lens' UpdateCloudFrontOriginAccessIdentityResponse (Maybe Text)
ucfoairsETag = lens _ucfoairsETag (\ s a -> s{_ucfoairsETag = a})

-- | The origin access identity's information.
ucfoairsCloudFrontOriginAccessIdentity :: Lens' UpdateCloudFrontOriginAccessIdentityResponse (Maybe CloudFrontOriginAccessIdentity)
ucfoairsCloudFrontOriginAccessIdentity = lens _ucfoairsCloudFrontOriginAccessIdentity (\ s a -> s{_ucfoairsCloudFrontOriginAccessIdentity = a})

-- | -- | The response status code.
ucfoairsResponseStatus :: Lens' UpdateCloudFrontOriginAccessIdentityResponse Int
ucfoairsResponseStatus = lens _ucfoairsResponseStatus (\ s a -> s{_ucfoairsResponseStatus = a})

instance NFData
           UpdateCloudFrontOriginAccessIdentityResponse
         where
