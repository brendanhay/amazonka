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

-- Module      : Network.AWS.KMS.GenerateDataKey
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Generates a secure data key. Data keys are used to encrypt and decrypt data.
-- They are wrapped by customer master keys.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateDataKey.html>
module Network.AWS.KMS.GenerateDataKey
    (
    -- * Request
      GenerateDataKey
    -- ** Request constructor
    , generateDataKey
    -- ** Request lenses
    , gdkEncryptionContext
    , gdkGrantTokens
    , gdkKeyId
    , gdkKeySpec
    , gdkNumberOfBytes

    -- * Response
    , GenerateDataKeyResponse
    -- ** Response constructor
    , generateDataKeyResponse
    -- ** Response lenses
    , gdkrCiphertextBlob
    , gdkrKeyId
    , gdkrPlaintext
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

data GenerateDataKey = GenerateDataKey
    { _gdkEncryptionContext :: Map Text Text
    , _gdkGrantTokens       :: List "GrantTokens" Text
    , _gdkKeyId             :: Text
    , _gdkKeySpec           :: Maybe DataKeySpec
    , _gdkNumberOfBytes     :: Maybe Nat
    } deriving (Eq, Show)

-- | 'GenerateDataKey' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdkEncryptionContext' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'gdkGrantTokens' @::@ ['Text']
--
-- * 'gdkKeyId' @::@ 'Text'
--
-- * 'gdkKeySpec' @::@ 'Maybe' 'DataKeySpec'
--
-- * 'gdkNumberOfBytes' @::@ 'Maybe' 'Natural'
--
generateDataKey :: Text -- ^ 'gdkKeyId'
                -> GenerateDataKey
generateDataKey p1 = GenerateDataKey
    { _gdkKeyId             = p1
    , _gdkEncryptionContext = mempty
    , _gdkNumberOfBytes     = Nothing
    , _gdkKeySpec           = Nothing
    , _gdkGrantTokens       = mempty
    }

-- | Name/value pair that contains additional data to be authenticated during the
-- encryption and decryption processes that use the key. This value is logged by
-- AWS CloudTrail to provide context around the data encrypted by the key.
--
gdkEncryptionContext :: Lens' GenerateDataKey (HashMap Text Text)
gdkEncryptionContext =
    lens _gdkEncryptionContext (\s a -> s { _gdkEncryptionContext = a })
        . _Map

-- | A list of grant tokens that represent grants which can be used to provide
-- long term permissions to generate a key.
--
gdkGrantTokens :: Lens' GenerateDataKey [Text]
gdkGrantTokens = lens _gdkGrantTokens (\s a -> s { _gdkGrantTokens = a }) . _List

-- | Unique identifier of the key. This can be an ARN, an alias, or a globally
-- unique identifier.
--
gdkKeyId :: Lens' GenerateDataKey Text
gdkKeyId = lens _gdkKeyId (\s a -> s { _gdkKeyId = a })

-- | Value that identifies the encryption algorithm and key size to generate a
-- data key for. Currently this can be AES_128 or AES_256.
--
gdkKeySpec :: Lens' GenerateDataKey (Maybe DataKeySpec)
gdkKeySpec = lens _gdkKeySpec (\s a -> s { _gdkKeySpec = a })

-- | Integer that contains the number of bytes to generate. Common values are 128,
-- 256, 512, 1024 and so on. 1024 is the current limit.
--
gdkNumberOfBytes :: Lens' GenerateDataKey (Maybe Natural)
gdkNumberOfBytes = lens _gdkNumberOfBytes (\s a -> s { _gdkNumberOfBytes = a }) . mapping _Nat

data GenerateDataKeyResponse = GenerateDataKeyResponse
    { _gdkrCiphertextBlob :: Maybe Base64
    , _gdkrKeyId          :: Maybe Text
    , _gdkrPlaintext      :: Maybe Base64
    } deriving (Eq, Show)

-- | 'GenerateDataKeyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdkrCiphertextBlob' @::@ 'Maybe' 'Base64'
--
-- * 'gdkrKeyId' @::@ 'Maybe' 'Text'
--
-- * 'gdkrPlaintext' @::@ 'Maybe' 'Base64'
--
generateDataKeyResponse :: GenerateDataKeyResponse
generateDataKeyResponse = GenerateDataKeyResponse
    { _gdkrCiphertextBlob = Nothing
    , _gdkrPlaintext      = Nothing
    , _gdkrKeyId          = Nothing
    }

-- | Ciphertext that contains the wrapped key. You must store the blob and
-- encryption context so that the ciphertext can be decrypted. You must provide
-- both the ciphertext blob and the encryption context.
--
gdkrCiphertextBlob :: Lens' GenerateDataKeyResponse (Maybe Base64)
gdkrCiphertextBlob =
    lens _gdkrCiphertextBlob (\s a -> s { _gdkrCiphertextBlob = a })

-- | System generated unique identifier for the key.
--
gdkrKeyId :: Lens' GenerateDataKeyResponse (Maybe Text)
gdkrKeyId = lens _gdkrKeyId (\s a -> s { _gdkrKeyId = a })

-- | Plaintext that contains the unwrapped key. Use this for encryption and
-- decryption and then remove it from memory as soon as possible.
--
gdkrPlaintext :: Lens' GenerateDataKeyResponse (Maybe Base64)
gdkrPlaintext = lens _gdkrPlaintext (\s a -> s { _gdkrPlaintext = a })

instance ToPath GenerateDataKey where
    toPath = const "/"

instance ToQuery GenerateDataKey where
    toQuery = const mempty

instance ToHeaders GenerateDataKey

instance ToJSON GenerateDataKey where
    toJSON GenerateDataKey{..} = object
        [ "KeyId"             .= _gdkKeyId
        , "EncryptionContext" .= _gdkEncryptionContext
        , "NumberOfBytes"     .= _gdkNumberOfBytes
        , "KeySpec"           .= _gdkKeySpec
        , "GrantTokens"       .= _gdkGrantTokens
        ]

instance AWSRequest GenerateDataKey where
    type Sv GenerateDataKey = KMS
    type Rs GenerateDataKey = GenerateDataKeyResponse

    request  = post "GenerateDataKey"
    response = jsonResponse

instance FromJSON GenerateDataKeyResponse where
    parseJSON = withObject "GenerateDataKeyResponse" $ \o -> GenerateDataKeyResponse
        <$> o .:? "CiphertextBlob"
        <*> o .:? "KeyId"
        <*> o .:? "Plaintext"
