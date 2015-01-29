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

-- Module      : Network.AWS.KMS.ReEncrypt
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

-- | Encrypts data on the server side with a new customer master key without
-- exposing the plaintext of the data on the client side. The data is first
-- decrypted and then encrypted. This operation can also be used to change the
-- encryption context of a ciphertext.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_ReEncrypt.html>
module Network.AWS.KMS.ReEncrypt
    (
    -- * Request
      ReEncrypt
    -- ** Request constructor
    , reEncrypt
    -- ** Request lenses
    , reCiphertextBlob
    , reDestinationEncryptionContext
    , reDestinationKeyId
    , reGrantTokens
    , reSourceEncryptionContext

    -- * Response
    , ReEncryptResponse
    -- ** Response constructor
    , reEncryptResponse
    -- ** Response lenses
    , rerCiphertextBlob
    , rerKeyId
    , rerSourceKeyId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

data ReEncrypt = ReEncrypt
    { _reCiphertextBlob               :: Base64
    , _reDestinationEncryptionContext :: Map Text Text
    , _reDestinationKeyId             :: Text
    , _reGrantTokens                  :: List "GrantTokens" Text
    , _reSourceEncryptionContext      :: Map Text Text
    } deriving (Eq, Read, Show)

-- | 'ReEncrypt' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reCiphertextBlob' @::@ 'Base64'
--
-- * 'reDestinationEncryptionContext' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'reDestinationKeyId' @::@ 'Text'
--
-- * 'reGrantTokens' @::@ ['Text']
--
-- * 'reSourceEncryptionContext' @::@ 'HashMap' 'Text' 'Text'
--
reEncrypt :: Base64 -- ^ 'reCiphertextBlob'
          -> Text -- ^ 'reDestinationKeyId'
          -> ReEncrypt
reEncrypt p1 p2 = ReEncrypt
    { _reCiphertextBlob               = p1
    , _reDestinationKeyId             = p2
    , _reSourceEncryptionContext      = mempty
    , _reDestinationEncryptionContext = mempty
    , _reGrantTokens                  = mempty
    }

-- | Ciphertext of the data to re-encrypt.
reCiphertextBlob :: Lens' ReEncrypt Base64
reCiphertextBlob = lens _reCiphertextBlob (\s a -> s { _reCiphertextBlob = a })

-- | Encryption context to be used when the data is re-encrypted.
reDestinationEncryptionContext :: Lens' ReEncrypt (HashMap Text Text)
reDestinationEncryptionContext =
    lens _reDestinationEncryptionContext
        (\s a -> s { _reDestinationEncryptionContext = a })
            . _Map

-- | A unique identifier for the customer master key used to re-encrypt the data.
-- This value can be a globally unique identifier, a fully specified ARN to
-- either an alias or a key, or an alias name prefixed by "alias/".  Key ARN
-- Example -
-- arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012 Alias ARN Example - arn:aws:kms:us-east-1:123456789012:/alias/MyAliasName
-- Globally Unique Key ID Example - 12345678-1234-1234-123456789012 Alias Name
-- Example - alias/MyAliasName
reDestinationKeyId :: Lens' ReEncrypt Text
reDestinationKeyId =
    lens _reDestinationKeyId (\s a -> s { _reDestinationKeyId = a })

-- | For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>.
reGrantTokens :: Lens' ReEncrypt [Text]
reGrantTokens = lens _reGrantTokens (\s a -> s { _reGrantTokens = a }) . _List

-- | Encryption context used to encrypt and decrypt the data specified in the 'CiphertextBlob' parameter.
reSourceEncryptionContext :: Lens' ReEncrypt (HashMap Text Text)
reSourceEncryptionContext =
    lens _reSourceEncryptionContext
        (\s a -> s { _reSourceEncryptionContext = a })
            . _Map

data ReEncryptResponse = ReEncryptResponse
    { _rerCiphertextBlob :: Maybe Base64
    , _rerKeyId          :: Maybe Text
    , _rerSourceKeyId    :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ReEncryptResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rerCiphertextBlob' @::@ 'Maybe' 'Base64'
--
-- * 'rerKeyId' @::@ 'Maybe' 'Text'
--
-- * 'rerSourceKeyId' @::@ 'Maybe' 'Text'
--
reEncryptResponse :: ReEncryptResponse
reEncryptResponse = ReEncryptResponse
    { _rerCiphertextBlob = Nothing
    , _rerSourceKeyId    = Nothing
    , _rerKeyId          = Nothing
    }

-- | The re-encrypted data. If you are using the CLI, the value is Base64 encoded.
-- Otherwise, it is not encoded.
rerCiphertextBlob :: Lens' ReEncryptResponse (Maybe Base64)
rerCiphertextBlob =
    lens _rerCiphertextBlob (\s a -> s { _rerCiphertextBlob = a })

-- | Unique identifier of the key used to re-encrypt the data.
rerKeyId :: Lens' ReEncryptResponse (Maybe Text)
rerKeyId = lens _rerKeyId (\s a -> s { _rerKeyId = a })

-- | Unique identifier of the key used to originally encrypt the data.
rerSourceKeyId :: Lens' ReEncryptResponse (Maybe Text)
rerSourceKeyId = lens _rerSourceKeyId (\s a -> s { _rerSourceKeyId = a })

instance ToPath ReEncrypt where
    toPath = const "/"

instance ToQuery ReEncrypt where
    toQuery = const mempty

instance ToHeaders ReEncrypt

instance ToJSON ReEncrypt where
    toJSON ReEncrypt{..} = object
        [ "CiphertextBlob"               .= _reCiphertextBlob
        , "SourceEncryptionContext"      .= _reSourceEncryptionContext
        , "DestinationKeyId"             .= _reDestinationKeyId
        , "DestinationEncryptionContext" .= _reDestinationEncryptionContext
        , "GrantTokens"                  .= _reGrantTokens
        ]

instance AWSRequest ReEncrypt where
    type Sv ReEncrypt = KMS
    type Rs ReEncrypt = ReEncryptResponse

    request  = post "ReEncrypt"
    response = jsonResponse

instance FromJSON ReEncryptResponse where
    parseJSON = withObject "ReEncryptResponse" $ \o -> ReEncryptResponse
        <$> o .:? "CiphertextBlob"
        <*> o .:? "KeyId"
        <*> o .:? "SourceKeyId"
