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
-- Module      : Network.AWS.CloudFront.GetPublicKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the public key information.
--
--
module Network.AWS.CloudFront.GetPublicKey
    (
    -- * Creating a Request
      getPublicKey
    , GetPublicKey
    -- * Request Lenses
    , gpkId

    -- * Destructuring the Response
    , getPublicKeyResponse
    , GetPublicKeyResponse
    -- * Response Lenses
    , gpkrsETag
    , gpkrsPublicKey
    , gpkrsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPublicKey' smart constructor.
newtype GetPublicKey = GetPublicKey'
  { _gpkId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpkId' - Request the ID for the public key.
getPublicKey
    :: Text -- ^ 'gpkId'
    -> GetPublicKey
getPublicKey pId_ = GetPublicKey' {_gpkId = pId_}


-- | Request the ID for the public key.
gpkId :: Lens' GetPublicKey Text
gpkId = lens _gpkId (\ s a -> s{_gpkId = a})

instance AWSRequest GetPublicKey where
        type Rs GetPublicKey = GetPublicKeyResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 GetPublicKeyResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (pure (fromEnum s)))

instance Hashable GetPublicKey where

instance NFData GetPublicKey where

instance ToHeaders GetPublicKey where
        toHeaders = const mempty

instance ToPath GetPublicKey where
        toPath GetPublicKey'{..}
          = mconcat ["/2017-10-30/public-key/", toBS _gpkId]

instance ToQuery GetPublicKey where
        toQuery = const mempty

-- | /See:/ 'getPublicKeyResponse' smart constructor.
data GetPublicKeyResponse = GetPublicKeyResponse'
  { _gpkrsETag           :: !(Maybe Text)
  , _gpkrsPublicKey      :: !(Maybe PublicKey)
  , _gpkrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPublicKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpkrsETag' - The current version of the public key. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'gpkrsPublicKey' - Return the public key.
--
-- * 'gpkrsResponseStatus' - -- | The response status code.
getPublicKeyResponse
    :: Int -- ^ 'gpkrsResponseStatus'
    -> GetPublicKeyResponse
getPublicKeyResponse pResponseStatus_ =
  GetPublicKeyResponse'
    { _gpkrsETag = Nothing
    , _gpkrsPublicKey = Nothing
    , _gpkrsResponseStatus = pResponseStatus_
    }


-- | The current version of the public key. For example: @E2QWRUHAPOMQZL@ .
gpkrsETag :: Lens' GetPublicKeyResponse (Maybe Text)
gpkrsETag = lens _gpkrsETag (\ s a -> s{_gpkrsETag = a})

-- | Return the public key.
gpkrsPublicKey :: Lens' GetPublicKeyResponse (Maybe PublicKey)
gpkrsPublicKey = lens _gpkrsPublicKey (\ s a -> s{_gpkrsPublicKey = a})

-- | -- | The response status code.
gpkrsResponseStatus :: Lens' GetPublicKeyResponse Int
gpkrsResponseStatus = lens _gpkrsResponseStatus (\ s a -> s{_gpkrsResponseStatus = a})

instance NFData GetPublicKeyResponse where
