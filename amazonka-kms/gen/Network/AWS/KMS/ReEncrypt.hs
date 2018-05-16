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
-- Module      : Network.AWS.KMS.ReEncrypt
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Encrypts data on the server side with a new customer master key (CMK) without exposing the plaintext of the data on the client side. The data is first decrypted and then reencrypted. You can also use this operation to change the encryption context of a ciphertext.
--
--
-- You can reencrypt data using CMKs in different AWS accounts.
--
-- Unlike other operations, @ReEncrypt@ is authorized twice, once as @ReEncryptFrom@ on the source CMK and once as @ReEncryptTo@ on the destination CMK. We recommend that you include the @"kms:ReEncrypt*"@ permission in your <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html key policies> to permit reencryption from or to the CMK. This permission is automatically included in the key policy when you create a CMK through the console, but you must include it manually when you create a CMK programmatically or when you set a key policy with the 'PutKeyPolicy' operation.
--
module Network.AWS.KMS.ReEncrypt
    (
    -- * Creating a Request
      reEncrypt
    , ReEncrypt
    -- * Request Lenses
    , reDestinationEncryptionContext
    , reSourceEncryptionContext
    , reGrantTokens
    , reCiphertextBlob
    , reDestinationKeyId

    -- * Destructuring the Response
    , reEncryptResponse
    , ReEncryptResponse
    -- * Response Lenses
    , rersSourceKeyId
    , rersKeyId
    , rersCiphertextBlob
    , rersResponseStatus
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'reEncrypt' smart constructor.
data ReEncrypt = ReEncrypt'
  { _reDestinationEncryptionContext :: !(Maybe (Map Text Text))
  , _reSourceEncryptionContext      :: !(Maybe (Map Text Text))
  , _reGrantTokens                  :: !(Maybe [Text])
  , _reCiphertextBlob               :: !Base64
  , _reDestinationKeyId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReEncrypt' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reDestinationEncryptionContext' - Encryption context to use when the data is reencrypted.
--
-- * 'reSourceEncryptionContext' - Encryption context used to encrypt and decrypt the data specified in the @CiphertextBlob@ parameter.
--
-- * 'reGrantTokens' - A list of grant tokens. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'reCiphertextBlob' - Ciphertext of the data to reencrypt.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'reDestinationKeyId' - A unique identifier for the CMK that is used to reencrypt the data. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with "alias/". To specify a CMK in a different AWS account, you must use the key ARN or alias ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
reEncrypt
    :: ByteString -- ^ 'reCiphertextBlob'
    -> Text -- ^ 'reDestinationKeyId'
    -> ReEncrypt
reEncrypt pCiphertextBlob_ pDestinationKeyId_ =
  ReEncrypt'
    { _reDestinationEncryptionContext = Nothing
    , _reSourceEncryptionContext = Nothing
    , _reGrantTokens = Nothing
    , _reCiphertextBlob = _Base64 # pCiphertextBlob_
    , _reDestinationKeyId = pDestinationKeyId_
    }


-- | Encryption context to use when the data is reencrypted.
reDestinationEncryptionContext :: Lens' ReEncrypt (HashMap Text Text)
reDestinationEncryptionContext = lens _reDestinationEncryptionContext (\ s a -> s{_reDestinationEncryptionContext = a}) . _Default . _Map

-- | Encryption context used to encrypt and decrypt the data specified in the @CiphertextBlob@ parameter.
reSourceEncryptionContext :: Lens' ReEncrypt (HashMap Text Text)
reSourceEncryptionContext = lens _reSourceEncryptionContext (\ s a -> s{_reSourceEncryptionContext = a}) . _Default . _Map

-- | A list of grant tokens. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens> in the /AWS Key Management Service Developer Guide/ .
reGrantTokens :: Lens' ReEncrypt [Text]
reGrantTokens = lens _reGrantTokens (\ s a -> s{_reGrantTokens = a}) . _Default . _Coerce

-- | Ciphertext of the data to reencrypt.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
reCiphertextBlob :: Lens' ReEncrypt ByteString
reCiphertextBlob = lens _reCiphertextBlob (\ s a -> s{_reCiphertextBlob = a}) . _Base64

-- | A unique identifier for the CMK that is used to reencrypt the data. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. When using an alias name, prefix it with "alias/". To specify a CMK in a different AWS account, you must use the key ARN or alias ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias/ExampleAlias@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' . To get the alias name and alias ARN, use 'ListAliases' .
reDestinationKeyId :: Lens' ReEncrypt Text
reDestinationKeyId = lens _reDestinationKeyId (\ s a -> s{_reDestinationKeyId = a})

instance AWSRequest ReEncrypt where
        type Rs ReEncrypt = ReEncryptResponse
        request = postJSON kms
        response
          = receiveJSON
              (\ s h x ->
                 ReEncryptResponse' <$>
                   (x .?> "SourceKeyId") <*> (x .?> "KeyId") <*>
                     (x .?> "CiphertextBlob")
                     <*> (pure (fromEnum s)))

instance Hashable ReEncrypt where

instance NFData ReEncrypt where

instance ToHeaders ReEncrypt where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.ReEncrypt" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ReEncrypt where
        toJSON ReEncrypt'{..}
          = object
              (catMaybes
                 [("DestinationEncryptionContext" .=) <$>
                    _reDestinationEncryptionContext,
                  ("SourceEncryptionContext" .=) <$>
                    _reSourceEncryptionContext,
                  ("GrantTokens" .=) <$> _reGrantTokens,
                  Just ("CiphertextBlob" .= _reCiphertextBlob),
                  Just ("DestinationKeyId" .= _reDestinationKeyId)])

instance ToPath ReEncrypt where
        toPath = const "/"

instance ToQuery ReEncrypt where
        toQuery = const mempty

-- | /See:/ 'reEncryptResponse' smart constructor.
data ReEncryptResponse = ReEncryptResponse'
  { _rersSourceKeyId    :: !(Maybe Text)
  , _rersKeyId          :: !(Maybe Text)
  , _rersCiphertextBlob :: !(Maybe Base64)
  , _rersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReEncryptResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rersSourceKeyId' - Unique identifier of the CMK used to originally encrypt the data.
--
-- * 'rersKeyId' - Unique identifier of the CMK used to reencrypt the data.
--
-- * 'rersCiphertextBlob' - The reencrypted data. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'rersResponseStatus' - -- | The response status code.
reEncryptResponse
    :: Int -- ^ 'rersResponseStatus'
    -> ReEncryptResponse
reEncryptResponse pResponseStatus_ =
  ReEncryptResponse'
    { _rersSourceKeyId = Nothing
    , _rersKeyId = Nothing
    , _rersCiphertextBlob = Nothing
    , _rersResponseStatus = pResponseStatus_
    }


-- | Unique identifier of the CMK used to originally encrypt the data.
rersSourceKeyId :: Lens' ReEncryptResponse (Maybe Text)
rersSourceKeyId = lens _rersSourceKeyId (\ s a -> s{_rersSourceKeyId = a})

-- | Unique identifier of the CMK used to reencrypt the data.
rersKeyId :: Lens' ReEncryptResponse (Maybe Text)
rersKeyId = lens _rersKeyId (\ s a -> s{_rersKeyId = a})

-- | The reencrypted data. When you use the HTTP API or the AWS CLI, the value is Base64-encoded. Otherwise, it is not encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
rersCiphertextBlob :: Lens' ReEncryptResponse (Maybe ByteString)
rersCiphertextBlob = lens _rersCiphertextBlob (\ s a -> s{_rersCiphertextBlob = a}) . mapping _Base64

-- | -- | The response status code.
rersResponseStatus :: Lens' ReEncryptResponse Int
rersResponseStatus = lens _rersResponseStatus (\ s a -> s{_rersResponseStatus = a})

instance NFData ReEncryptResponse where
