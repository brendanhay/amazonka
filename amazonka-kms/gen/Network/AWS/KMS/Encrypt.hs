{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Encrypts plaintext into ciphertext by using a customer master key. The 'Encrypt'
-- function has two primary use cases:  You can encrypt up to 4 KB of arbitrary
-- data such as an RSA key, a database password, or other sensitive customer
-- information. If you are moving encrypted data from one region to another, you
-- can use this API to encrypt in the new region the plaintext data key that was
-- used to encrypt the data in the original region. This provides you with an
-- encrypted copy of the data key that can be decrypted in the new region and
-- used there to decrypt the encrypted data.
--
-- Unless you are moving encrypted data from one region to another, you don't
-- use this function to encrypt a generated data key within a region. You
-- retrieve data keys already encrypted by calling the 'GenerateDataKey' or 'GenerateDataKeyWithoutPlaintext' function. Data keys don't need to be encrypted again by calling 'Encrypt'.
--
-- If you want to encrypt data locally in your application, you can use the 'GenerateDataKey' function to return a plaintext data encryption key and a copy of the key
-- encrypted under the customer master key (CMK) of your choosing.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_Encrypt.html>
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
    , _eGrantTokens       :: List "GrantTokens" Text
    , _eKeyId             :: Text
    , _ePlaintext         :: Base64
    } deriving (Eq, Read, Show)

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

-- | Name/value pair that specifies the encryption context to be used for
-- authenticated encryption. If used here, the same value must be supplied to
-- the 'Decrypt' API or decryption will fail. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/encrypt-context.html EncryptionContext>.
eEncryptionContext :: Lens' Encrypt (HashMap Text Text)
eEncryptionContext =
    lens _eEncryptionContext (\s a -> s { _eEncryptionContext = a })
        . _Map

-- | For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>.
eGrantTokens :: Lens' Encrypt [Text]
eGrantTokens = lens _eGrantTokens (\s a -> s { _eGrantTokens = a }) . _List

-- | A unique identifier for the customer master key. This value can be a globally
-- unique identifier, a fully specified ARN to either an alias or a key, or an
-- alias name prefixed by "alias/".  Key ARN Example -
-- arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012 Alias ARN Example - arn:aws:kms:us-east-1:123456789012:/alias/MyAliasName
-- Globally Unique Key ID Example - 12345678-1234-1234-123456789012 Alias Name
-- Example - alias/MyAliasName
eKeyId :: Lens' Encrypt Text
eKeyId = lens _eKeyId (\s a -> s { _eKeyId = a })

-- | Data to be encrypted.
ePlaintext :: Lens' Encrypt Base64
ePlaintext = lens _ePlaintext (\s a -> s { _ePlaintext = a })

data EncryptResponse = EncryptResponse
    { _erCiphertextBlob :: Maybe Base64
    , _erKeyId          :: Maybe Text
    } deriving (Eq, Read, Show)

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

-- | The encrypted plaintext. If you are using the CLI, the value is Base64
-- encoded. Otherwise, it is not encoded.
erCiphertextBlob :: Lens' EncryptResponse (Maybe Base64)
erCiphertextBlob = lens _erCiphertextBlob (\s a -> s { _erCiphertextBlob = a })

-- | The ID of the key used during encryption.
erKeyId :: Lens' EncryptResponse (Maybe Text)
erKeyId = lens _erKeyId (\s a -> s { _erKeyId = a })

instance ToPath Encrypt where
    toPath = const "/"

instance ToQuery Encrypt where
    toQuery = const mempty

instance ToHeaders Encrypt

instance ToJSON Encrypt where
    toJSON Encrypt{..} = object
        [ "KeyId"             .= _eKeyId
        , "Plaintext"         .= _ePlaintext
        , "EncryptionContext" .= _eEncryptionContext
        , "GrantTokens"       .= _eGrantTokens
        ]

instance AWSRequest Encrypt where
    type Sv Encrypt = KMS
    type Rs Encrypt = EncryptResponse

    request  = post "Encrypt"
    response = jsonResponse

instance FromJSON EncryptResponse where
    parseJSON = withObject "EncryptResponse" $ \o -> EncryptResponse
        <$> o .:? "CiphertextBlob"
        <*> o .:? "KeyId"
