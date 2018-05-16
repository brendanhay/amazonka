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
-- Module      : Network.AWS.CloudFront.GetPublicKeyConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return public key configuration informaation
--
--
module Network.AWS.CloudFront.GetPublicKeyConfig
    (
    -- * Creating a Request
      getPublicKeyConfig
    , GetPublicKeyConfig
    -- * Request Lenses
    , gpkcId

    -- * Destructuring the Response
    , getPublicKeyConfigResponse
    , GetPublicKeyConfigResponse
    -- * Response Lenses
    , gpkcrsETag
    , gpkcrsPublicKeyConfig
    , gpkcrsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPublicKeyConfig' smart constructor.
newtype GetPublicKeyConfig = GetPublicKeyConfig'
  { _gpkcId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPublicKeyConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpkcId' - Request the ID for the public key configuration.
getPublicKeyConfig
    :: Text -- ^ 'gpkcId'
    -> GetPublicKeyConfig
getPublicKeyConfig pId_ = GetPublicKeyConfig' {_gpkcId = pId_}


-- | Request the ID for the public key configuration.
gpkcId :: Lens' GetPublicKeyConfig Text
gpkcId = lens _gpkcId (\ s a -> s{_gpkcId = a})

instance AWSRequest GetPublicKeyConfig where
        type Rs GetPublicKeyConfig =
             GetPublicKeyConfigResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 GetPublicKeyConfigResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (pure (fromEnum s)))

instance Hashable GetPublicKeyConfig where

instance NFData GetPublicKeyConfig where

instance ToHeaders GetPublicKeyConfig where
        toHeaders = const mempty

instance ToPath GetPublicKeyConfig where
        toPath GetPublicKeyConfig'{..}
          = mconcat
              ["/2017-10-30/public-key/", toBS _gpkcId, "/config"]

instance ToQuery GetPublicKeyConfig where
        toQuery = const mempty

-- | /See:/ 'getPublicKeyConfigResponse' smart constructor.
data GetPublicKeyConfigResponse = GetPublicKeyConfigResponse'
  { _gpkcrsETag            :: !(Maybe Text)
  , _gpkcrsPublicKeyConfig :: !(Maybe PublicKeyConfig)
  , _gpkcrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPublicKeyConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpkcrsETag' - The current version of the public key configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'gpkcrsPublicKeyConfig' - Return the result for the public key configuration.
--
-- * 'gpkcrsResponseStatus' - -- | The response status code.
getPublicKeyConfigResponse
    :: Int -- ^ 'gpkcrsResponseStatus'
    -> GetPublicKeyConfigResponse
getPublicKeyConfigResponse pResponseStatus_ =
  GetPublicKeyConfigResponse'
    { _gpkcrsETag = Nothing
    , _gpkcrsPublicKeyConfig = Nothing
    , _gpkcrsResponseStatus = pResponseStatus_
    }


-- | The current version of the public key configuration. For example: @E2QWRUHAPOMQZL@ .
gpkcrsETag :: Lens' GetPublicKeyConfigResponse (Maybe Text)
gpkcrsETag = lens _gpkcrsETag (\ s a -> s{_gpkcrsETag = a})

-- | Return the result for the public key configuration.
gpkcrsPublicKeyConfig :: Lens' GetPublicKeyConfigResponse (Maybe PublicKeyConfig)
gpkcrsPublicKeyConfig = lens _gpkcrsPublicKeyConfig (\ s a -> s{_gpkcrsPublicKeyConfig = a})

-- | -- | The response status code.
gpkcrsResponseStatus :: Lens' GetPublicKeyConfigResponse Int
gpkcrsResponseStatus = lens _gpkcrsResponseStatus (\ s a -> s{_gpkcrsResponseStatus = a})

instance NFData GetPublicKeyConfigResponse where
