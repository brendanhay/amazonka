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
-- Module      : Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a data encryption key encrypted under a customer master key (CMK). This operation is identical to 'GenerateDataKey' but returns only the encrypted copy of the data key.
--
--
-- To perform this operation on a CMK in a different AWS account, specify the key ARN or alias ARN in the value of the KeyId parameter.
--
-- This operation is useful in a system that has multiple components with different degrees of trust. For example, consider a system that stores encrypted data in containers. Each container stores the encrypted data and an encrypted copy of the data key. One component of the system, called the /control plane/ , creates new containers. When it creates a new container, it uses this operation (@GenerateDataKeyWithoutPlaintext@ ) to get an encrypted data key and then stores it in the container. Later, a different component of the system, called the /data plane/ , puts encrypted data into the containers. To do this, it passes the encrypted data key to the 'Decrypt' operation, then uses the returned plaintext data key to encrypt data, and finally stores the encrypted data in the container. In this system, the control plane never sees the plaintext data key.
--
module Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
    (
    -- * Creating a Request
      generateDataKeyWithoutPlaintext
    , GenerateDataKeyWithoutPlaintext
    -- * Request Lenses
    , gdkwpKeySpec
    , gdkwpEncryptionContext
    , gdkwpNumberOfBytes
    , gdkwpGrantTokens
    , gdkwpKeyId

    -- * Destructuring the Response
    , generateDataKeyWithoutPlaintextResponse
    , GenerateDataKeyWithoutPlaintextResponse
    -- * Response Lenses
    , gdkwprsKeyId
    , gdkwprsCiphertextBlob
    , gdkwprsResponseStatus
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'generateDataKeyWithoutPlaintext' smart constructor.
data GenerateDataKeyWithoutPlaintext = GenerateDataKeyWithoutPlaintext'
  { _gdkwpKeySpec           :: !(Maybe DataKeySpec)
  , _gdkwpEncryptionContext :: !(Maybe (Map Text Text))
  , _gdkwpNumberOfBytes     :: !(Maybe Nat)
  , _gdkwpGrantTokens       :: !(Maybe [Text])
  , _gdkwpKeyId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GenerateDataKeyWithoutPlaintext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdkwpKeySpec' - The length of the data encryption key. Use @AES_128@ to generate a 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
--
-- * 'gdkwpEncryptionContext' - A set of key-value pairs that represents additional authenticated data. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/encryption-context.html Encryption Context> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'gdkwpNumberOfBytes' - The length of the data encryption key in bytes. For example, use the value 64 to generate a 512-bit data key (64 bytes is 512 bits). For common key lengths (128-bit and 256-bit symmetric keys), we recommend that you use the @KeySpec@ field instead of this one.
--
-- * 'gdkwpGrantTokens' - A list of grant tokens. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'gdkwpKeyId' - The identifier of the customer master key (CMK) under which to generate and encrypt the data encryption key. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with "alias/". To specify a CMK in a different AWS account, you must use the key ARN or alias ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
generateDataKeyWithoutPlaintext
    :: Text -- ^ 'gdkwpKeyId'
    -> GenerateDataKeyWithoutPlaintext
generateDataKeyWithoutPlaintext pKeyId_ =
  GenerateDataKeyWithoutPlaintext'
    { _gdkwpKeySpec = Nothing
    , _gdkwpEncryptionContext = Nothing
    , _gdkwpNumberOfBytes = Nothing
    , _gdkwpGrantTokens = Nothing
    , _gdkwpKeyId = pKeyId_
    }


-- | The length of the data encryption key. Use @AES_128@ to generate a 128-bit symmetric key, or @AES_256@ to generate a 256-bit symmetric key.
gdkwpKeySpec :: Lens' GenerateDataKeyWithoutPlaintext (Maybe DataKeySpec)
gdkwpKeySpec = lens _gdkwpKeySpec (\ s a -> s{_gdkwpKeySpec = a})

-- | A set of key-value pairs that represents additional authenticated data. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/encryption-context.html Encryption Context> in the /AWS Key Management Service Developer Guide/ .
gdkwpEncryptionContext :: Lens' GenerateDataKeyWithoutPlaintext (HashMap Text Text)
gdkwpEncryptionContext = lens _gdkwpEncryptionContext (\ s a -> s{_gdkwpEncryptionContext = a}) . _Default . _Map

-- | The length of the data encryption key in bytes. For example, use the value 64 to generate a 512-bit data key (64 bytes is 512 bits). For common key lengths (128-bit and 256-bit symmetric keys), we recommend that you use the @KeySpec@ field instead of this one.
gdkwpNumberOfBytes :: Lens' GenerateDataKeyWithoutPlaintext (Maybe Natural)
gdkwpNumberOfBytes = lens _gdkwpNumberOfBytes (\ s a -> s{_gdkwpNumberOfBytes = a}) . mapping _Nat

-- | A list of grant tokens. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
gdkwpGrantTokens :: Lens' GenerateDataKeyWithoutPlaintext [Text]
gdkwpGrantTokens = lens _gdkwpGrantTokens (\ s a -> s{_gdkwpGrantTokens = a}) . _Default . _Coerce

-- | The identifier of the customer master key (CMK) under which to generate and encrypt the data encryption key. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with "alias/". To specify a CMK in a different AWS account, you must use the key ARN or alias ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
gdkwpKeyId :: Lens' GenerateDataKeyWithoutPlaintext Text
gdkwpKeyId = lens _gdkwpKeyId (\ s a -> s{_gdkwpKeyId = a})

instance AWSRequest GenerateDataKeyWithoutPlaintext
         where
        type Rs GenerateDataKeyWithoutPlaintext =
             GenerateDataKeyWithoutPlaintextResponse
        request = postJSON kms
        response
          = receiveJSON
              (\ s h x ->
                 GenerateDataKeyWithoutPlaintextResponse' <$>
                   (x .?> "KeyId") <*> (x .?> "CiphertextBlob") <*>
                     (pure (fromEnum s)))

instance Hashable GenerateDataKeyWithoutPlaintext
         where

instance NFData GenerateDataKeyWithoutPlaintext where

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
              (catMaybes
                 [("KeySpec" .=) <$> _gdkwpKeySpec,
                  ("EncryptionContext" .=) <$> _gdkwpEncryptionContext,
                  ("NumberOfBytes" .=) <$> _gdkwpNumberOfBytes,
                  ("GrantTokens" .=) <$> _gdkwpGrantTokens,
                  Just ("KeyId" .= _gdkwpKeyId)])

instance ToPath GenerateDataKeyWithoutPlaintext where
        toPath = const "/"

instance ToQuery GenerateDataKeyWithoutPlaintext
         where
        toQuery = const mempty

-- | /See:/ 'generateDataKeyWithoutPlaintextResponse' smart constructor.
data GenerateDataKeyWithoutPlaintextResponse = GenerateDataKeyWithoutPlaintextResponse'
  { _gdkwprsKeyId          :: !(Maybe Text)
  , _gdkwprsCiphertextBlob :: !(Maybe Base64)
  , _gdkwprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GenerateDataKeyWithoutPlaintextResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdkwprsKeyId' - The identifier of the CMK under which the data encryption key was generated and encrypted.
--
-- * 'gdkwprsCiphertextBlob' - The encrypted data encryption key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'gdkwprsResponseStatus' - -- | The response status code.
generateDataKeyWithoutPlaintextResponse
    :: Int -- ^ 'gdkwprsResponseStatus'
    -> GenerateDataKeyWithoutPlaintextResponse
generateDataKeyWithoutPlaintextResponse pResponseStatus_ =
  GenerateDataKeyWithoutPlaintextResponse'
    { _gdkwprsKeyId = Nothing
    , _gdkwprsCiphertextBlob = Nothing
    , _gdkwprsResponseStatus = pResponseStatus_
    }


-- | The identifier of the CMK under which the data encryption key was generated and encrypted.
gdkwprsKeyId :: Lens' GenerateDataKeyWithoutPlaintextResponse (Maybe Text)
gdkwprsKeyId = lens _gdkwprsKeyId (\ s a -> s{_gdkwprsKeyId = a})

-- | The encrypted data encryption key. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
gdkwprsCiphertextBlob :: Lens' GenerateDataKeyWithoutPlaintextResponse (Maybe ByteString)
gdkwprsCiphertextBlob = lens _gdkwprsCiphertextBlob (\ s a -> s{_gdkwprsCiphertextBlob = a}) . mapping _Base64

-- | -- | The response status code.
gdkwprsResponseStatus :: Lens' GenerateDataKeyWithoutPlaintextResponse Int
gdkwprsResponseStatus = lens _gdkwprsResponseStatus (\ s a -> s{_gdkwprsResponseStatus = a})

instance NFData
           GenerateDataKeyWithoutPlaintextResponse
         where
