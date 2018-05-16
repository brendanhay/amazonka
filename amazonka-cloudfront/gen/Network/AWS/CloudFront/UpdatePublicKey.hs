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
-- Module      : Network.AWS.CloudFront.UpdatePublicKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update public key information. Note that the only value you can change is the comment.
--
--
module Network.AWS.CloudFront.UpdatePublicKey
    (
    -- * Creating a Request
      updatePublicKey
    , UpdatePublicKey
    -- * Request Lenses
    , upkIfMatch
    , upkPublicKeyConfig
    , upkId

    -- * Destructuring the Response
    , updatePublicKeyResponse
    , UpdatePublicKeyResponse
    -- * Response Lenses
    , upkrsETag
    , upkrsPublicKey
    , upkrsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updatePublicKey' smart constructor.
data UpdatePublicKey = UpdatePublicKey'
  { _upkIfMatch         :: !(Maybe Text)
  , _upkPublicKeyConfig :: !PublicKeyConfig
  , _upkId              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdatePublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upkIfMatch' - The value of the @ETag@ header that you received when retrieving the public key to update. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'upkPublicKeyConfig' - Request to update public key information.
--
-- * 'upkId' - ID of the public key to be updated.
updatePublicKey
    :: PublicKeyConfig -- ^ 'upkPublicKeyConfig'
    -> Text -- ^ 'upkId'
    -> UpdatePublicKey
updatePublicKey pPublicKeyConfig_ pId_ =
  UpdatePublicKey'
    { _upkIfMatch = Nothing
    , _upkPublicKeyConfig = pPublicKeyConfig_
    , _upkId = pId_
    }


-- | The value of the @ETag@ header that you received when retrieving the public key to update. For example: @E2QWRUHAPOMQZL@ .
upkIfMatch :: Lens' UpdatePublicKey (Maybe Text)
upkIfMatch = lens _upkIfMatch (\ s a -> s{_upkIfMatch = a})

-- | Request to update public key information.
upkPublicKeyConfig :: Lens' UpdatePublicKey PublicKeyConfig
upkPublicKeyConfig = lens _upkPublicKeyConfig (\ s a -> s{_upkPublicKeyConfig = a})

-- | ID of the public key to be updated.
upkId :: Lens' UpdatePublicKey Text
upkId = lens _upkId (\ s a -> s{_upkId = a})

instance AWSRequest UpdatePublicKey where
        type Rs UpdatePublicKey = UpdatePublicKeyResponse
        request = putXML cloudFront
        response
          = receiveXML
              (\ s h x ->
                 UpdatePublicKeyResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (pure (fromEnum s)))

instance Hashable UpdatePublicKey where

instance NFData UpdatePublicKey where

instance ToElement UpdatePublicKey where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2017-10-30/}PublicKeyConfig"
              .
              _upkPublicKeyConfig

instance ToHeaders UpdatePublicKey where
        toHeaders UpdatePublicKey'{..}
          = mconcat ["If-Match" =# _upkIfMatch]

instance ToPath UpdatePublicKey where
        toPath UpdatePublicKey'{..}
          = mconcat
              ["/2017-10-30/public-key/", toBS _upkId, "/config"]

instance ToQuery UpdatePublicKey where
        toQuery = const mempty

-- | /See:/ 'updatePublicKeyResponse' smart constructor.
data UpdatePublicKeyResponse = UpdatePublicKeyResponse'
  { _upkrsETag           :: !(Maybe Text)
  , _upkrsPublicKey      :: !(Maybe PublicKey)
  , _upkrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdatePublicKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upkrsETag' - The current version of the update public key result. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'upkrsPublicKey' - Return the results of updating the public key.
--
-- * 'upkrsResponseStatus' - -- | The response status code.
updatePublicKeyResponse
    :: Int -- ^ 'upkrsResponseStatus'
    -> UpdatePublicKeyResponse
updatePublicKeyResponse pResponseStatus_ =
  UpdatePublicKeyResponse'
    { _upkrsETag = Nothing
    , _upkrsPublicKey = Nothing
    , _upkrsResponseStatus = pResponseStatus_
    }


-- | The current version of the update public key result. For example: @E2QWRUHAPOMQZL@ .
upkrsETag :: Lens' UpdatePublicKeyResponse (Maybe Text)
upkrsETag = lens _upkrsETag (\ s a -> s{_upkrsETag = a})

-- | Return the results of updating the public key.
upkrsPublicKey :: Lens' UpdatePublicKeyResponse (Maybe PublicKey)
upkrsPublicKey = lens _upkrsPublicKey (\ s a -> s{_upkrsPublicKey = a})

-- | -- | The response status code.
upkrsResponseStatus :: Lens' UpdatePublicKeyResponse Int
upkrsResponseStatus = lens _upkrsResponseStatus (\ s a -> s{_upkrsResponseStatus = a})

instance NFData UpdatePublicKeyResponse where
