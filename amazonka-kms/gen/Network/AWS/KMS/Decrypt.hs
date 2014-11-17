{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.KMS.Decrypt
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Decrypts ciphertext. Ciphertext is plaintext that has been previously
-- encrypted by using the Encrypt function.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_Decrypt.html>
module Network.AWS.KMS.Decrypt
    (
    -- * Request
      Decrypt
    -- ** Request constructor
    , decrypt
    -- ** Request lenses
    , dCiphertextBlob
    , dEncryptionContext
    , dGrantTokens

    -- * Response
    , DecryptResponse
    -- ** Response constructor
    , decryptResponse
    -- ** Response lenses
    , drKeyId
    , drPlaintext
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

data Decrypt = Decrypt
    { _dCiphertextBlob    :: Base64
    , _dEncryptionContext :: Map Text Text
    , _dGrantTokens       :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'Decrypt' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dCiphertextBlob' @::@ 'Base64'
--
-- * 'dEncryptionContext' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'dGrantTokens' @::@ ['Text']
--
decrypt :: Base64 -- ^ 'dCiphertextBlob'
        -> Decrypt
decrypt p1 = Decrypt
    { _dCiphertextBlob    = p1
    , _dEncryptionContext = mempty
    , _dGrantTokens       = mempty
    }

-- | Ciphertext including metadata.
dCiphertextBlob :: Lens' Decrypt Base64
dCiphertextBlob = lens _dCiphertextBlob (\s a -> s { _dCiphertextBlob = a })

-- | The encryption context. If this was specified in the Encrypt function, it
-- must be specified here or the decryption operation will fail. For more
-- information, see Encryption Context.
dEncryptionContext :: Lens' Decrypt (HashMap Text Text)
dEncryptionContext =
    lens _dEncryptionContext (\s a -> s { _dEncryptionContext = a })
        . _Map

-- | A list of grant tokens that represent grants which can be used to provide
-- long term permissions to perform decryption.
dGrantTokens :: Lens' Decrypt [Text]
dGrantTokens = lens _dGrantTokens (\s a -> s { _dGrantTokens = a })

data DecryptResponse = DecryptResponse
    { _drKeyId     :: Maybe Text
    , _drPlaintext :: Maybe Base64
    } deriving (Eq, Show, Generic)

-- | 'DecryptResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drKeyId' @::@ 'Maybe' 'Text'
--
-- * 'drPlaintext' @::@ 'Maybe' 'Base64'
--
decryptResponse :: DecryptResponse
decryptResponse = DecryptResponse
    { _drKeyId     = Nothing
    , _drPlaintext = Nothing
    }

-- | Unique identifier created by the system for the key. This value is always
-- returned as long as no errors are encountered during the operation.
drKeyId :: Lens' DecryptResponse (Maybe Text)
drKeyId = lens _drKeyId (\s a -> s { _drKeyId = a })

-- | Decrypted plaintext data. This value may not be returned if the customer
-- master key is not available or if you didn't have permission to use it.
drPlaintext :: Lens' DecryptResponse (Maybe Base64)
drPlaintext = lens _drPlaintext (\s a -> s { _drPlaintext = a })

instance ToPath Decrypt where
    toPath = const "/"

instance ToQuery Decrypt where
    toQuery = const mempty

instance ToHeaders Decrypt

instance ToJSON Decrypt where
    toJSON Decrypt{..} = object
        [ "CiphertextBlob"    .= _dCiphertextBlob
        , "EncryptionContext" .= _dEncryptionContext
        , "GrantTokens"       .= _dGrantTokens
        ]

instance AWSRequest Decrypt where
    type Sv Decrypt = KMS
    type Rs Decrypt = DecryptResponse

    request  = post "Decrypt"
    response = jsonResponse

instance FromJSON DecryptResponse where
    parseJSON = withObject "DecryptResponse" $ \o -> DecryptResponse
        <$> o .: "KeyId"
        <*> o .: "Plaintext"
