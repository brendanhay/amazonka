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

-- Module      : Network.AWS.KMS.Decrypt
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

-- | Decrypts ciphertext. Ciphertext is plaintext that has been previously
-- encrypted by using any of the following functions:  'GenerateDataKey' 'GenerateDataKeyWithoutPlaintext' 'Encrypt'
--
-- Note that if a caller has been granted access permissions to all keys
-- (through, for example, IAM user policies that grant 'Decrypt' permission on all
-- resources), then ciphertext encrypted by using keys in other accounts where
-- the key grants access to the caller can be decrypted. To remedy this, we
-- recommend that you do not grant 'Decrypt' access in an IAM user policy. Instead
-- grant 'Decrypt' access only in key policies. If you must grant 'Decrypt' access
-- in an IAM user policy, you should scope the resource to specific keys or to
-- specific trusted accounts.
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

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

data Decrypt = Decrypt
    { _dCiphertextBlob    :: Base64
    , _dEncryptionContext :: Map Text Text
    , _dGrantTokens       :: List "GrantTokens" Text
    } deriving (Eq, Read, Show)

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

-- | Ciphertext to be decrypted. The blob includes metadata.
dCiphertextBlob :: Lens' Decrypt Base64
dCiphertextBlob = lens _dCiphertextBlob (\s a -> s { _dCiphertextBlob = a })

-- | The encryption context. If this was specified in the 'Encrypt' function, it
-- must be specified here or the decryption operation will fail. For more
-- information, see <http://docs.aws.amazon.com/kms/latest/developerguide/encrypt-context.html Encryption Context>.
dEncryptionContext :: Lens' Decrypt (HashMap Text Text)
dEncryptionContext =
    lens _dEncryptionContext (\s a -> s { _dEncryptionContext = a })
        . _Map

-- | For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>.
dGrantTokens :: Lens' Decrypt [Text]
dGrantTokens = lens _dGrantTokens (\s a -> s { _dGrantTokens = a }) . _List

data DecryptResponse = DecryptResponse
    { _drKeyId     :: Maybe Text
    , _drPlaintext :: Maybe Base64
    } deriving (Eq, Read, Show)

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

-- | ARN of the key used to perform the decryption. This value is returned if no
-- errors are encountered during the operation.
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
        <$> o .:? "KeyId"
        <*> o .:? "Plaintext"
