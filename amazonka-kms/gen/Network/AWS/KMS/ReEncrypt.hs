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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Encrypts data on the server side with a new customer master key without
-- exposing the plaintext of the data on the client side. The data is first
-- decrypted and then encrypted. This operation can also be used to change
-- the encryption context of a ciphertext.
--
-- Unlike other actions, 'ReEncrypt' is authorized twice - once as
-- 'ReEncryptFrom' on the source key and once as 'ReEncryptTo' on the
-- destination key. We therefore recommend that you include the
-- '\"action\":\"kms:ReEncrypt*\"' statement in your key policies to permit
-- re-encryption from or to the key. The statement is included
-- automatically when you authorize use of the key through the console but
-- must be included manually when you set a policy by using the
-- < PutKeyPolicy> function.
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

import           Network.AWS.KMS.Types
import           Network.AWS.KMS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'reEncrypt' smart constructor.
data ReEncrypt = ReEncrypt'
    { _reDestinationEncryptionContext :: !(Maybe (Map Text Text))
    , _reSourceEncryptionContext      :: !(Maybe (Map Text Text))
    , _reGrantTokens                  :: !(Maybe [Text])
    , _reCiphertextBlob               :: !Base64
    , _reDestinationKeyId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReEncrypt' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reDestinationEncryptionContext'
--
-- * 'reSourceEncryptionContext'
--
-- * 'reGrantTokens'
--
-- * 'reCiphertextBlob'
--
-- * 'reDestinationKeyId'
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

-- | Encryption context to be used when the data is re-encrypted.
reDestinationEncryptionContext :: Lens' ReEncrypt (HashMap Text Text)
reDestinationEncryptionContext = lens _reDestinationEncryptionContext (\ s a -> s{_reDestinationEncryptionContext = a}) . _Default . _Map;

-- | Encryption context used to encrypt and decrypt the data specified in the
-- 'CiphertextBlob' parameter.
reSourceEncryptionContext :: Lens' ReEncrypt (HashMap Text Text)
reSourceEncryptionContext = lens _reSourceEncryptionContext (\ s a -> s{_reSourceEncryptionContext = a}) . _Default . _Map;

-- | A list of grant tokens.
--
-- For more information, go to
-- <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>
-- in the /AWS Key Management Service Developer Guide/.
reGrantTokens :: Lens' ReEncrypt [Text]
reGrantTokens = lens _reGrantTokens (\ s a -> s{_reGrantTokens = a}) . _Default . _Coerce;

-- | Ciphertext of the data to re-encrypt.
--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data,
-- despite what the AWS documentation might say.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
reCiphertextBlob :: Lens' ReEncrypt ByteString
reCiphertextBlob = lens _reCiphertextBlob (\ s a -> s{_reCiphertextBlob = a}) . _Base64;

-- | A unique identifier for the customer master key used to re-encrypt the
-- data. This value can be a globally unique identifier, a fully specified
-- ARN to either an alias or a key, or an alias name prefixed by
-- \"alias\/\".
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Alias ARN Example -
--     arn:aws:kms:us-east-1:123456789012:alias\/MyAliasName
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
-- -   Alias Name Example - alias\/MyAliasName
reDestinationKeyId :: Lens' ReEncrypt Text
reDestinationKeyId = lens _reDestinationKeyId (\ s a -> s{_reDestinationKeyId = a});

instance AWSRequest ReEncrypt where
        type Rs ReEncrypt = ReEncryptResponse
        request = postJSON kMS
        response
          = receiveJSON
              (\ s h x ->
                 ReEncryptResponse' <$>
                   (x .?> "SourceKeyId") <*> (x .?> "KeyId") <*>
                     (x .?> "CiphertextBlob")
                     <*> (pure (fromEnum s)))

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReEncryptResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rersSourceKeyId'
--
-- * 'rersKeyId'
--
-- * 'rersCiphertextBlob'
--
-- * 'rersResponseStatus'
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

-- | Unique identifier of the key used to originally encrypt the data.
rersSourceKeyId :: Lens' ReEncryptResponse (Maybe Text)
rersSourceKeyId = lens _rersSourceKeyId (\ s a -> s{_rersSourceKeyId = a});

-- | Unique identifier of the key used to re-encrypt the data.
rersKeyId :: Lens' ReEncryptResponse (Maybe Text)
rersKeyId = lens _rersKeyId (\ s a -> s{_rersKeyId = a});

-- | The re-encrypted data. If you are using the CLI, the value is Base64
-- encoded. Otherwise, it is not encoded.
--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data,
-- despite what the AWS documentation might say.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
rersCiphertextBlob :: Lens' ReEncryptResponse (Maybe ByteString)
rersCiphertextBlob = lens _rersCiphertextBlob (\ s a -> s{_rersCiphertextBlob = a}) . mapping _Base64;

-- | The response status code.
rersResponseStatus :: Lens' ReEncryptResponse Int
rersResponseStatus = lens _rersResponseStatus (\ s a -> s{_rersResponseStatus = a});
