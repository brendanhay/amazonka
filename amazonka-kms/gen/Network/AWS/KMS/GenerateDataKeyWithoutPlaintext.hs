{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a data key encrypted by a customer master key without the
-- plaintext copy of that key. Otherwise, this API functions exactly like
-- GenerateDataKey. You can use this API to, for example, satisfy an audit
-- requirement that an encrypted key be made available without exposing the
-- plaintext copy of that key.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateDataKeyWithoutPlaintext.html>
module Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
    (
    -- * Request
      GenerateDataKeyWithoutPlaintext
    -- ** Request constructor
    , generateDataKeyWithoutPlaintext
    -- ** Request lenses
    , gdkwpKeySpec
    , gdkwpEncryptionContext
    , gdkwpGrantTokens
    , gdkwpKeyId
    , gdkwpNumberOfBytes

    -- * Response
    , GenerateDataKeyWithoutPlaintextResponse
    -- ** Response constructor
    , generateDataKeyWithoutPlaintextResponse
    -- ** Response lenses
    , gdkwprKeyId
    , gdkwprCiphertextBlob
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.KMS.Types

-- | /See:/ 'generateDataKeyWithoutPlaintext' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdkwpKeySpec'
--
-- * 'gdkwpEncryptionContext'
--
-- * 'gdkwpGrantTokens'
--
-- * 'gdkwpKeyId'
--
-- * 'gdkwpNumberOfBytes'
data GenerateDataKeyWithoutPlaintext = GenerateDataKeyWithoutPlaintext'{_gdkwpKeySpec :: Maybe DataKeySpec, _gdkwpEncryptionContext :: HashMap Text Text, _gdkwpGrantTokens :: [Text], _gdkwpKeyId :: Text, _gdkwpNumberOfBytes :: Nat} deriving (Eq, Read, Show)

-- | 'GenerateDataKeyWithoutPlaintext' smart constructor.
generateDataKeyWithoutPlaintext :: Text -> Natural -> GenerateDataKeyWithoutPlaintext
generateDataKeyWithoutPlaintext pKeyId pNumberOfBytes = GenerateDataKeyWithoutPlaintext'{_gdkwpKeySpec = Nothing, _gdkwpEncryptionContext = mempty, _gdkwpGrantTokens = mempty, _gdkwpKeyId = pKeyId, _gdkwpNumberOfBytes = _Nat # pNumberOfBytes};

-- | Value that identifies the encryption algorithm and key size. Currently
-- this can be AES_128 or AES_256.
gdkwpKeySpec :: Lens' GenerateDataKeyWithoutPlaintext (Maybe DataKeySpec)
gdkwpKeySpec = lens _gdkwpKeySpec (\ s a -> s{_gdkwpKeySpec = a});

-- | Name:value pair that contains additional data to be authenticated during
-- the encryption and decryption processes.
gdkwpEncryptionContext :: Lens' GenerateDataKeyWithoutPlaintext (HashMap Text Text)
gdkwpEncryptionContext = lens _gdkwpEncryptionContext (\ s a -> s{_gdkwpEncryptionContext = a}) . _Coerce;

-- | For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>.
gdkwpGrantTokens :: Lens' GenerateDataKeyWithoutPlaintext [Text]
gdkwpGrantTokens = lens _gdkwpGrantTokens (\ s a -> s{_gdkwpGrantTokens = a});

-- | A unique identifier for the customer master key. This value can be a
-- globally unique identifier, a fully specified ARN to either an alias or
-- a key, or an alias name prefixed by \"alias\/\".
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Alias ARN Example -
--     arn:aws:kms:us-east-1:123456789012:alias\/MyAliasName
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
-- -   Alias Name Example - alias\/MyAliasName
gdkwpKeyId :: Lens' GenerateDataKeyWithoutPlaintext Text
gdkwpKeyId = lens _gdkwpKeyId (\ s a -> s{_gdkwpKeyId = a});

-- | Integer that contains the number of bytes to generate. Common values are
-- 128, 256, 512, 1024 and so on. We recommend that you use the @KeySpec@
-- parameter instead.
gdkwpNumberOfBytes :: Lens' GenerateDataKeyWithoutPlaintext Natural
gdkwpNumberOfBytes = lens _gdkwpNumberOfBytes (\ s a -> s{_gdkwpNumberOfBytes = a}) . _Nat;

instance AWSRequest GenerateDataKeyWithoutPlaintext
         where
        type Sv GenerateDataKeyWithoutPlaintext = KMS
        type Rs GenerateDataKeyWithoutPlaintext =
             GenerateDataKeyWithoutPlaintextResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GenerateDataKeyWithoutPlaintextResponse' <$>
                   x .:> "KeyId" <*> x .:> "CiphertextBlob")

instance ToHeaders GenerateDataKeyWithoutPlaintext
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.GenerateDataKeyWithoutPlaintext" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GenerateDataKeyWithoutPlaintext where
        toJSON GenerateDataKeyWithoutPlaintext'{..}
          = object
              ["KeySpec" .= _gdkwpKeySpec,
               "EncryptionContext" .= _gdkwpEncryptionContext,
               "GrantTokens" .= _gdkwpGrantTokens,
               "KeyId" .= _gdkwpKeyId,
               "NumberOfBytes" .= _gdkwpNumberOfBytes]

instance ToPath GenerateDataKeyWithoutPlaintext where
        toPath = const "/"

instance ToQuery GenerateDataKeyWithoutPlaintext
         where
        toQuery = const mempty

-- | /See:/ 'generateDataKeyWithoutPlaintextResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdkwprKeyId'
--
-- * 'gdkwprCiphertextBlob'
data GenerateDataKeyWithoutPlaintextResponse = GenerateDataKeyWithoutPlaintextResponse'{_gdkwprKeyId :: Text, _gdkwprCiphertextBlob :: Base64} deriving (Eq, Read, Show)

-- | 'GenerateDataKeyWithoutPlaintextResponse' smart constructor.
generateDataKeyWithoutPlaintextResponse :: Text -> Base64 -> GenerateDataKeyWithoutPlaintextResponse
generateDataKeyWithoutPlaintextResponse pKeyId pCiphertextBlob = GenerateDataKeyWithoutPlaintextResponse'{_gdkwprKeyId = pKeyId, _gdkwprCiphertextBlob = pCiphertextBlob};

-- | System generated unique identifier of the key to be used to decrypt the
-- encrypted copy of the data key.
gdkwprKeyId :: Lens' GenerateDataKeyWithoutPlaintextResponse Text
gdkwprKeyId = lens _gdkwprKeyId (\ s a -> s{_gdkwprKeyId = a});

-- | Ciphertext that contains the wrapped data key. You must store the blob
-- and encryption context so that the key can be used in a future decrypt
-- operation.
--
-- If you are using the CLI, the value is Base64 encoded. Otherwise, it is
-- not encoded.
gdkwprCiphertextBlob :: Lens' GenerateDataKeyWithoutPlaintextResponse Base64
gdkwprCiphertextBlob = lens _gdkwprCiphertextBlob (\ s a -> s{_gdkwprCiphertextBlob = a});
