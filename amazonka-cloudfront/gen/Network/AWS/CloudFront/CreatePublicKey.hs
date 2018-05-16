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
-- Module      : Network.AWS.CloudFront.CreatePublicKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add a new public key to CloudFront to use, for example, for field-level encryption. You can add a maximum of 10 public keys with one AWS account.
--
--
module Network.AWS.CloudFront.CreatePublicKey
    (
    -- * Creating a Request
      createPublicKey
    , CreatePublicKey
    -- * Request Lenses
    , cpkPublicKeyConfig

    -- * Destructuring the Response
    , createPublicKeyResponse
    , CreatePublicKeyResponse
    -- * Response Lenses
    , cpkrsETag
    , cpkrsLocation
    , cpkrsPublicKey
    , cpkrsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createPublicKey' smart constructor.
newtype CreatePublicKey = CreatePublicKey'
  { _cpkPublicKeyConfig :: PublicKeyConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpkPublicKeyConfig' - The request to add a public key to CloudFront.
createPublicKey
    :: PublicKeyConfig -- ^ 'cpkPublicKeyConfig'
    -> CreatePublicKey
createPublicKey pPublicKeyConfig_ =
  CreatePublicKey' {_cpkPublicKeyConfig = pPublicKeyConfig_}


-- | The request to add a public key to CloudFront.
cpkPublicKeyConfig :: Lens' CreatePublicKey PublicKeyConfig
cpkPublicKeyConfig = lens _cpkPublicKeyConfig (\ s a -> s{_cpkPublicKeyConfig = a})

instance AWSRequest CreatePublicKey where
        type Rs CreatePublicKey = CreatePublicKeyResponse
        request = postXML cloudFront
        response
          = receiveXML
              (\ s h x ->
                 CreatePublicKeyResponse' <$>
                   (h .#? "ETag") <*> (h .#? "Location") <*>
                     (parseXML x)
                     <*> (pure (fromEnum s)))

instance Hashable CreatePublicKey where

instance NFData CreatePublicKey where

instance ToElement CreatePublicKey where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2017-10-30/}PublicKeyConfig"
              .
              _cpkPublicKeyConfig

instance ToHeaders CreatePublicKey where
        toHeaders = const mempty

instance ToPath CreatePublicKey where
        toPath = const "/2017-10-30/public-key"

instance ToQuery CreatePublicKey where
        toQuery = const mempty

-- | /See:/ 'createPublicKeyResponse' smart constructor.
data CreatePublicKeyResponse = CreatePublicKeyResponse'
  { _cpkrsETag           :: !(Maybe Text)
  , _cpkrsLocation       :: !(Maybe Text)
  , _cpkrsPublicKey      :: !(Maybe PublicKey)
  , _cpkrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePublicKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpkrsETag' - The current version of the public key. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'cpkrsLocation' - The fully qualified URI of the new public key resource just created. For example: @https://cloudfront.amazonaws.com/2010-11-01/cloudfront-public-key/EDFDVBD632BHDS5@ .
--
-- * 'cpkrsPublicKey' - Returned when you add a public key.
--
-- * 'cpkrsResponseStatus' - -- | The response status code.
createPublicKeyResponse
    :: Int -- ^ 'cpkrsResponseStatus'
    -> CreatePublicKeyResponse
createPublicKeyResponse pResponseStatus_ =
  CreatePublicKeyResponse'
    { _cpkrsETag = Nothing
    , _cpkrsLocation = Nothing
    , _cpkrsPublicKey = Nothing
    , _cpkrsResponseStatus = pResponseStatus_
    }


-- | The current version of the public key. For example: @E2QWRUHAPOMQZL@ .
cpkrsETag :: Lens' CreatePublicKeyResponse (Maybe Text)
cpkrsETag = lens _cpkrsETag (\ s a -> s{_cpkrsETag = a})

-- | The fully qualified URI of the new public key resource just created. For example: @https://cloudfront.amazonaws.com/2010-11-01/cloudfront-public-key/EDFDVBD632BHDS5@ .
cpkrsLocation :: Lens' CreatePublicKeyResponse (Maybe Text)
cpkrsLocation = lens _cpkrsLocation (\ s a -> s{_cpkrsLocation = a})

-- | Returned when you add a public key.
cpkrsPublicKey :: Lens' CreatePublicKeyResponse (Maybe PublicKey)
cpkrsPublicKey = lens _cpkrsPublicKey (\ s a -> s{_cpkrsPublicKey = a})

-- | -- | The response status code.
cpkrsResponseStatus :: Lens' CreatePublicKeyResponse Int
cpkrsResponseStatus = lens _cpkrsResponseStatus (\ s a -> s{_cpkrsResponseStatus = a})

instance NFData CreatePublicKeyResponse where
