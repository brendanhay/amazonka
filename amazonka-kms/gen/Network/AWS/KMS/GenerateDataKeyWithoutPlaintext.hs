{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a key wrapped by a customer master key without the plaintext copy
-- of that key. To retrieve the plaintext, see GenerateDataKey.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateDataKeyWithoutPlaintext.html>
module Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
    (
    -- * Request
      GenerateDataKeyWithoutPlaintext
    -- ** Request constructor
    , generateDataKeyWithoutPlaintext
    -- ** Request lenses
    , gdkwpEncryptionContext
    , gdkwpGrantTokens
    , gdkwpKeyId
    , gdkwpKeySpec
    , gdkwpNumberOfBytes

    -- * Response
    , GenerateDataKeyWithoutPlaintextResponse
    -- ** Response constructor
    , generateDataKeyWithoutPlaintextResponse
    -- ** Response lenses
    , gdkwprCiphertextBlob
    , gdkwprKeyId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

data GenerateDataKeyWithoutPlaintext = GenerateDataKeyWithoutPlaintext
    { _gdkwpEncryptionContext :: Map Text Text
    , _gdkwpGrantTokens       :: [Text]
    , _gdkwpKeyId             :: Text
    , _gdkwpKeySpec           :: Maybe Text
    , _gdkwpNumberOfBytes     :: Maybe Nat
    } deriving (Eq, Show, Generic)

-- | 'GenerateDataKeyWithoutPlaintext' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdkwpEncryptionContext' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'gdkwpGrantTokens' @::@ ['Text']
--
-- * 'gdkwpKeyId' @::@ 'Text'
--
-- * 'gdkwpKeySpec' @::@ 'Maybe' 'Text'
--
-- * 'gdkwpNumberOfBytes' @::@ 'Maybe' 'Natural'
--
generateDataKeyWithoutPlaintext :: Text -- ^ 'gdkwpKeyId'
                                -> GenerateDataKeyWithoutPlaintext
generateDataKeyWithoutPlaintext p1 = GenerateDataKeyWithoutPlaintext
    { _gdkwpKeyId             = p1
    , _gdkwpEncryptionContext = mempty
    , _gdkwpKeySpec           = Nothing
    , _gdkwpNumberOfBytes     = Nothing
    , _gdkwpGrantTokens       = mempty
    }

-- | Name:value pair that contains additional data to be authenticated during
-- the encryption and decryption processes.
gdkwpEncryptionContext :: Lens' GenerateDataKeyWithoutPlaintext (HashMap Text Text)
gdkwpEncryptionContext =
    lens _gdkwpEncryptionContext (\s a -> s { _gdkwpEncryptionContext = a })
        . _Map

-- | A list of grant tokens that represent grants which can be used to provide
-- long term permissions to generate a key.
gdkwpGrantTokens :: Lens' GenerateDataKeyWithoutPlaintext [Text]
gdkwpGrantTokens = lens _gdkwpGrantTokens (\s a -> s { _gdkwpGrantTokens = a })

-- | Unique identifier of the key. This can be an ARN, an alias, or a globally
-- unique identifier.
gdkwpKeyId :: Lens' GenerateDataKeyWithoutPlaintext Text
gdkwpKeyId = lens _gdkwpKeyId (\s a -> s { _gdkwpKeyId = a })

-- | Value that identifies the encryption algorithm and key size. Currently
-- this can be AES_128 or AES_256.
gdkwpKeySpec :: Lens' GenerateDataKeyWithoutPlaintext (Maybe Text)
gdkwpKeySpec = lens _gdkwpKeySpec (\s a -> s { _gdkwpKeySpec = a })

-- | Integer that contains the number of bytes to generate. Common values are
-- 128, 256, 512, 1024 and so on.
gdkwpNumberOfBytes :: Lens' GenerateDataKeyWithoutPlaintext (Maybe Natural)
gdkwpNumberOfBytes =
    lens _gdkwpNumberOfBytes (\s a -> s { _gdkwpNumberOfBytes = a })
        . mapping _Nat

data GenerateDataKeyWithoutPlaintextResponse = GenerateDataKeyWithoutPlaintextResponse
    { _gdkwprCiphertextBlob :: Maybe Base64
    , _gdkwprKeyId          :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'GenerateDataKeyWithoutPlaintextResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdkwprCiphertextBlob' @::@ 'Maybe' 'Base64'
--
-- * 'gdkwprKeyId' @::@ 'Maybe' 'Text'
--
generateDataKeyWithoutPlaintextResponse :: GenerateDataKeyWithoutPlaintextResponse
generateDataKeyWithoutPlaintextResponse = GenerateDataKeyWithoutPlaintextResponse
    { _gdkwprCiphertextBlob = Nothing
    , _gdkwprKeyId          = Nothing
    }

-- | Ciphertext that contains the wrapped key. You must store the blob and
-- encryption context so that the key can be used in a future operation.
gdkwprCiphertextBlob :: Lens' GenerateDataKeyWithoutPlaintextResponse (Maybe Base64)
gdkwprCiphertextBlob =
    lens _gdkwprCiphertextBlob (\s a -> s { _gdkwprCiphertextBlob = a })

-- | System generated unique identifier for the key.
gdkwprKeyId :: Lens' GenerateDataKeyWithoutPlaintextResponse (Maybe Text)
gdkwprKeyId = lens _gdkwprKeyId (\s a -> s { _gdkwprKeyId = a })

instance ToPath GenerateDataKeyWithoutPlaintext where
    toPath = const "/"

instance ToQuery GenerateDataKeyWithoutPlaintext where
    toQuery = const mempty

instance ToHeaders GenerateDataKeyWithoutPlaintext
instance ToJSON GenerateDataKeyWithoutPlaintext where
    toJSON = genericToJSON jsonOptions

instance AWSRequest GenerateDataKeyWithoutPlaintext where
    type Sv GenerateDataKeyWithoutPlaintext = KMS
    type Rs GenerateDataKeyWithoutPlaintext = GenerateDataKeyWithoutPlaintextResponse

    request  = post "GenerateDataKeyWithoutPlaintext"
    response = jsonResponse

instance FromJSON GenerateDataKeyWithoutPlaintextResponse where
    parseJSON = genericParseJSON jsonOptions
