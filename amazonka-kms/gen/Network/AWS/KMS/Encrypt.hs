{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.KMS.Encrypt
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Encrypts plaintext into ciphertext by using a customer master key.
module Network.AWS.KMS.Encrypt
    (
    -- * Request
      Encrypt
    -- ** Request constructor
    , encrypt
    -- ** Request lenses
    , eEncryptionContext
    , eGrantTokens
    , eKeyId
    , ePlaintext

    -- * Response
    , EncryptResponse
    -- ** Response constructor
    , encryptResponse
    -- ** Response lenses
    , erCiphertextBlob
    , erKeyId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

data Encrypt = Encrypt
    { _eEncryptionContext :: Map Text Text
    , _eGrantTokens       :: [Text]
    , _eKeyId             :: Text
    , _ePlaintext         :: Base64
    } deriving (Eq, Show, Generic)

-- | 'Encrypt' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eEncryptionContext' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'eGrantTokens' @::@ ['Text']
--
-- * 'eKeyId' @::@ 'Text'
--
-- * 'ePlaintext' @::@ 'Base64'
--
encrypt :: Text -- ^ 'eKeyId'
        -> Base64 -- ^ 'ePlaintext'
        -> Encrypt
encrypt p1 p2 = Encrypt
    { _eKeyId             = p1
    , _ePlaintext         = p2
    , _eEncryptionContext = mempty
    , _eGrantTokens       = mempty
    }

-- | Name:value pair that specifies the encryption context to be used for
-- authenticated encryption. For more information, see Authenticated
-- Encryption.
eEncryptionContext :: Lens' Encrypt (HashMap Text Text)
eEncryptionContext =
    lens _eEncryptionContext (\s a -> s { _eEncryptionContext = a })
        . _Map

-- | A list of grant tokens that represent grants which can be used to provide
-- long term permissions to perform encryption.
eGrantTokens :: Lens' Encrypt [Text]
eGrantTokens = lens _eGrantTokens (\s a -> s { _eGrantTokens = a })

-- | Unique identifier of the customer master. This can be an ARN, an alias,
-- or the Key ID.
eKeyId :: Lens' Encrypt Text
eKeyId = lens _eKeyId (\s a -> s { _eKeyId = a })

-- | Data to be encrypted.
ePlaintext :: Lens' Encrypt Base64
ePlaintext = lens _ePlaintext (\s a -> s { _ePlaintext = a })

data EncryptResponse = EncryptResponse
    { _erCiphertextBlob :: Maybe Base64
    , _erKeyId          :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'EncryptResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'erCiphertextBlob' @::@ 'Maybe' 'Base64'
--
-- * 'erKeyId' @::@ 'Maybe' 'Text'
--
encryptResponse :: EncryptResponse
encryptResponse = EncryptResponse
    { _erCiphertextBlob = Nothing
    , _erKeyId          = Nothing
    }

-- | The encrypted plaintext.
erCiphertextBlob :: Lens' EncryptResponse (Maybe Base64)
erCiphertextBlob = lens _erCiphertextBlob (\s a -> s { _erCiphertextBlob = a })

-- | The ID of the key used during encryption.
erKeyId :: Lens' EncryptResponse (Maybe Text)
erKeyId = lens _erKeyId (\s a -> s { _erKeyId = a })

instance AWSRequest Encrypt where
    type Sv Encrypt = KMS
    type Rs Encrypt = EncryptResponse

    request  = post
    response = jsonResponse

instance FromJSON EncryptResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath Encrypt where
    toPath = const "/"

instance ToHeaders Encrypt

instance ToQuery Encrypt where
    toQuery = const mempty

instance ToJSON Encrypt where
    toJSON = genericToJSON jsonOptions
