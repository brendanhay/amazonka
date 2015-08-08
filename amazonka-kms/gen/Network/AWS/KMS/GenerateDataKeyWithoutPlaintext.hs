{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a data key encrypted by a customer master key without the
-- plaintext copy of that key. Otherwise, this API functions exactly like
-- GenerateDataKey. You can use this API to, for example, satisfy an audit
-- requirement that an encrypted key be made available without exposing the
-- plaintext copy of that key.
--
-- /See:/ <http://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateDataKeyWithoutPlaintext.html AWS API Reference> for GenerateDataKeyWithoutPlaintext.
module Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
    (
    -- * Creating a Request
      GenerateDataKeyWithoutPlaintext
    , generateDataKeyWithoutPlaintext
    -- * Request Lenses
    , gdkwpKeySpec
    , gdkwpEncryptionContext
    , gdkwpNumberOfBytes
    , gdkwpGrantTokens
    , gdkwpKeyId

    -- * Destructuring the Response
    , GenerateDataKeyWithoutPlaintextResponse
    , generateDataKeyWithoutPlaintextResponse
    -- * Response Lenses
    , gdkwprsKeyId
    , gdkwprsCiphertextBlob
    , gdkwprsStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'generateDataKeyWithoutPlaintext' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdkwpKeySpec'
--
-- * 'gdkwpEncryptionContext'
--
-- * 'gdkwpNumberOfBytes'
--
-- * 'gdkwpGrantTokens'
--
-- * 'gdkwpKeyId'
data GenerateDataKeyWithoutPlaintext = GenerateDataKeyWithoutPlaintext'
    { _gdkwpKeySpec           :: !(Maybe DataKeySpec)
    , _gdkwpEncryptionContext :: !(Maybe (Map Text Text))
    , _gdkwpNumberOfBytes     :: !(Maybe Nat)
    , _gdkwpGrantTokens       :: !(Maybe [Text])
    , _gdkwpKeyId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GenerateDataKeyWithoutPlaintext' smart constructor.
generateDataKeyWithoutPlaintext :: Text -> GenerateDataKeyWithoutPlaintext
generateDataKeyWithoutPlaintext pKeyId_ =
    GenerateDataKeyWithoutPlaintext'
    { _gdkwpKeySpec = Nothing
    , _gdkwpEncryptionContext = Nothing
    , _gdkwpNumberOfBytes = Nothing
    , _gdkwpGrantTokens = Nothing
    , _gdkwpKeyId = pKeyId_
    }

-- | Value that identifies the encryption algorithm and key size. Currently
-- this can be AES_128 or AES_256.
gdkwpKeySpec :: Lens' GenerateDataKeyWithoutPlaintext (Maybe DataKeySpec)
gdkwpKeySpec = lens _gdkwpKeySpec (\ s a -> s{_gdkwpKeySpec = a});

-- | Name:value pair that contains additional data to be authenticated during
-- the encryption and decryption processes.
gdkwpEncryptionContext :: Lens' GenerateDataKeyWithoutPlaintext (HashMap Text Text)
gdkwpEncryptionContext = lens _gdkwpEncryptionContext (\ s a -> s{_gdkwpEncryptionContext = a}) . _Default . _Map;

-- | Integer that contains the number of bytes to generate. Common values are
-- 128, 256, 512, 1024 and so on. We recommend that you use the @KeySpec@
-- parameter instead.
gdkwpNumberOfBytes :: Lens' GenerateDataKeyWithoutPlaintext (Maybe Natural)
gdkwpNumberOfBytes = lens _gdkwpNumberOfBytes (\ s a -> s{_gdkwpNumberOfBytes = a}) . mapping _Nat;

-- | For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>.
gdkwpGrantTokens :: Lens' GenerateDataKeyWithoutPlaintext [Text]
gdkwpGrantTokens = lens _gdkwpGrantTokens (\ s a -> s{_gdkwpGrantTokens = a}) . _Default . _Coerce;

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
                   (x .?> "KeyId") <*> (x .?> "CiphertextBlob") <*>
                     (pure (fromEnum s)))

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
               "NumberOfBytes" .= _gdkwpNumberOfBytes,
               "GrantTokens" .= _gdkwpGrantTokens,
               "KeyId" .= _gdkwpKeyId]

instance ToPath GenerateDataKeyWithoutPlaintext where
        toPath = const "/"

instance ToQuery GenerateDataKeyWithoutPlaintext
         where
        toQuery = const mempty

-- | /See:/ 'generateDataKeyWithoutPlaintextResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdkwprsKeyId'
--
-- * 'gdkwprsCiphertextBlob'
--
-- * 'gdkwprsStatus'
data GenerateDataKeyWithoutPlaintextResponse = GenerateDataKeyWithoutPlaintextResponse'
    { _gdkwprsKeyId          :: !(Maybe Text)
    , _gdkwprsCiphertextBlob :: !(Maybe Base64)
    , _gdkwprsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GenerateDataKeyWithoutPlaintextResponse' smart constructor.
generateDataKeyWithoutPlaintextResponse :: Int -> GenerateDataKeyWithoutPlaintextResponse
generateDataKeyWithoutPlaintextResponse pStatus_ =
    GenerateDataKeyWithoutPlaintextResponse'
    { _gdkwprsKeyId = Nothing
    , _gdkwprsCiphertextBlob = Nothing
    , _gdkwprsStatus = pStatus_
    }

-- | System generated unique identifier of the key to be used to decrypt the
-- encrypted copy of the data key.
gdkwprsKeyId :: Lens' GenerateDataKeyWithoutPlaintextResponse (Maybe Text)
gdkwprsKeyId = lens _gdkwprsKeyId (\ s a -> s{_gdkwprsKeyId = a});

-- | Ciphertext that contains the wrapped data key. You must store the blob
-- and encryption context so that the key can be used in a future decrypt
-- operation.
--
-- If you are using the CLI, the value is Base64 encoded. Otherwise, it is
-- not encoded.
gdkwprsCiphertextBlob :: Lens' GenerateDataKeyWithoutPlaintextResponse (Maybe ByteString)
gdkwprsCiphertextBlob = lens _gdkwprsCiphertextBlob (\ s a -> s{_gdkwprsCiphertextBlob = a}) . mapping _Base64;

-- | Undocumented member.
gdkwprsStatus :: Lens' GenerateDataKeyWithoutPlaintextResponse Int
gdkwprsStatus = lens _gdkwprsStatus (\ s a -> s{_gdkwprsStatus = a});
