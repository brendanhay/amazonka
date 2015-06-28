{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.KMS.ReEncrypt
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

-- | Encrypts data on the server side with a new customer master key without
-- exposing the plaintext of the data on the client side. The data is first
-- decrypted and then encrypted. This operation can also be used to change
-- the encryption context of a ciphertext.
--
-- Unlike other actions, @ReEncrypt@ is authorized twice - once as
-- @ReEncryptFrom@ on the source key and once as @ReEncryptTo@ on the
-- destination key. We therefore recommend that you include the
-- @\"action\":\"kms:ReEncrypt*\"@ statement in your key policies to permit
-- re-encryption from or to the key. The statement is included
-- automatically when you authorize use of the key through the console but
-- must be included manually when you set a policy by using the
-- PutKeyPolicy function.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_ReEncrypt.html>
module Network.AWS.KMS.ReEncrypt
    (
    -- * Request
      ReEncrypt
    -- ** Request constructor
    , reEncrypt
    -- ** Request lenses
    , reDestinationEncryptionContext
    , reSourceEncryptionContext
    , reGrantTokens
    , reCiphertextBlob
    , reDestinationKeyId

    -- * Response
    , ReEncryptResponse
    -- ** Response constructor
    , reEncryptResponse
    -- ** Response lenses
    , rerSourceKeyId
    , rerKeyId
    , rerCiphertextBlob
    , rerStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'reEncrypt' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
data ReEncrypt = ReEncrypt'
    { _reDestinationEncryptionContext :: !(Maybe (Map Text Text))
    , _reSourceEncryptionContext      :: !(Maybe (Map Text Text))
    , _reGrantTokens                  :: !(Maybe [Text])
    , _reCiphertextBlob               :: !Base64
    , _reDestinationKeyId             :: !Text
    } deriving (Eq,Read,Show)

-- | 'ReEncrypt' smart constructor.
reEncrypt :: Base64 -> Text -> ReEncrypt
reEncrypt pCiphertextBlob pDestinationKeyId =
    ReEncrypt'
    { _reDestinationEncryptionContext = Nothing
    , _reSourceEncryptionContext = Nothing
    , _reGrantTokens = Nothing
    , _reCiphertextBlob = pCiphertextBlob
    , _reDestinationKeyId = pDestinationKeyId
    }

-- | Encryption context to be used when the data is re-encrypted.
reDestinationEncryptionContext :: Lens' ReEncrypt (HashMap Text Text)
reDestinationEncryptionContext = lens _reDestinationEncryptionContext (\ s a -> s{_reDestinationEncryptionContext = a}) . _Default . _Map;

-- | Encryption context used to encrypt and decrypt the data specified in the
-- @CiphertextBlob@ parameter.
reSourceEncryptionContext :: Lens' ReEncrypt (HashMap Text Text)
reSourceEncryptionContext = lens _reSourceEncryptionContext (\ s a -> s{_reSourceEncryptionContext = a}) . _Default . _Map;

-- | For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>.
reGrantTokens :: Lens' ReEncrypt [Text]
reGrantTokens = lens _reGrantTokens (\ s a -> s{_reGrantTokens = a}) . _Default;

-- | Ciphertext of the data to re-encrypt.
reCiphertextBlob :: Lens' ReEncrypt Base64
reCiphertextBlob = lens _reCiphertextBlob (\ s a -> s{_reCiphertextBlob = a});

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
        type Sv ReEncrypt = KMS
        type Rs ReEncrypt = ReEncryptResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ReEncryptResponse' <$>
                   (x .?> "SourceKeyId") <*> (x .?> "KeyId") <*>
                     (x .?> "CiphertextBlob")
                     <*> (pure s))

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
              ["DestinationEncryptionContext" .=
                 _reDestinationEncryptionContext,
               "SourceEncryptionContext" .=
                 _reSourceEncryptionContext,
               "GrantTokens" .= _reGrantTokens,
               "CiphertextBlob" .= _reCiphertextBlob,
               "DestinationKeyId" .= _reDestinationKeyId]

instance ToPath ReEncrypt where
        toPath = const "/"

instance ToQuery ReEncrypt where
        toQuery = const mempty

-- | /See:/ 'reEncryptResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rerSourceKeyId'
--
-- * 'rerKeyId'
--
-- * 'rerCiphertextBlob'
--
-- * 'rerStatus'
data ReEncryptResponse = ReEncryptResponse'
    { _rerSourceKeyId    :: !(Maybe Text)
    , _rerKeyId          :: !(Maybe Text)
    , _rerCiphertextBlob :: !(Maybe Base64)
    , _rerStatus         :: !Status
    } deriving (Eq,Show)

-- | 'ReEncryptResponse' smart constructor.
reEncryptResponse :: Status -> ReEncryptResponse
reEncryptResponse pStatus =
    ReEncryptResponse'
    { _rerSourceKeyId = Nothing
    , _rerKeyId = Nothing
    , _rerCiphertextBlob = Nothing
    , _rerStatus = pStatus
    }

-- | Unique identifier of the key used to originally encrypt the data.
rerSourceKeyId :: Lens' ReEncryptResponse (Maybe Text)
rerSourceKeyId = lens _rerSourceKeyId (\ s a -> s{_rerSourceKeyId = a});

-- | Unique identifier of the key used to re-encrypt the data.
rerKeyId :: Lens' ReEncryptResponse (Maybe Text)
rerKeyId = lens _rerKeyId (\ s a -> s{_rerKeyId = a});

-- | The re-encrypted data. If you are using the CLI, the value is Base64
-- encoded. Otherwise, it is not encoded.
rerCiphertextBlob :: Lens' ReEncryptResponse (Maybe Base64)
rerCiphertextBlob = lens _rerCiphertextBlob (\ s a -> s{_rerCiphertextBlob = a});

-- | FIXME: Undocumented member.
rerStatus :: Lens' ReEncryptResponse Status
rerStatus = lens _rerStatus (\ s a -> s{_rerStatus = a});
