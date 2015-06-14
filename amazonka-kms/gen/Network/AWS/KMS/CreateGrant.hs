{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.KMS.CreateGrant
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

-- | Adds a grant to a key to specify who can access the key and under what
-- conditions. Grants are alternate permission mechanisms to key policies.
-- For more information about grants, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/grants.html Grants>
-- in the developer guide. If a grant is absent, access to the key is
-- evaluated based on IAM policies attached to the user.
--
-- 1.  ListGrants
-- 2.  RetireGrant
-- 3.  RevokeGrant
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_CreateGrant.html>
module Network.AWS.KMS.CreateGrant
    (
    -- * Request
      CreateGrant
    -- ** Request constructor
    , createGrant
    -- ** Request lenses
    , cgConstraints
    , cgGrantTokens
    , cgOperations
    , cgKeyId
    , cgGranteePrincipal
    , cgRetiringPrincipal

    -- * Response
    , CreateGrantResponse
    -- ** Response constructor
    , createGrantResponse
    -- ** Response lenses
    , cgrGrantId
    , cgrGrantToken
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.KMS.Types

-- | /See:/ 'createGrant' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cgConstraints'
--
-- * 'cgGrantTokens'
--
-- * 'cgOperations'
--
-- * 'cgKeyId'
--
-- * 'cgGranteePrincipal'
--
-- * 'cgRetiringPrincipal'
data CreateGrant = CreateGrant'{_cgConstraints :: Maybe GrantConstraints, _cgGrantTokens :: [Text], _cgOperations :: [GrantOperation], _cgKeyId :: Text, _cgGranteePrincipal :: Text, _cgRetiringPrincipal :: Text} deriving (Eq, Read, Show)

-- | 'CreateGrant' smart constructor.
createGrant :: Text -> Text -> Text -> CreateGrant
createGrant pKeyId pGranteePrincipal pRetiringPrincipal = CreateGrant'{_cgConstraints = Nothing, _cgGrantTokens = mempty, _cgOperations = mempty, _cgKeyId = pKeyId, _cgGranteePrincipal = pGranteePrincipal, _cgRetiringPrincipal = pRetiringPrincipal};

-- | Specifies the conditions under which the actions specified by the
-- @Operations@ parameter are allowed.
cgConstraints :: Lens' CreateGrant (Maybe GrantConstraints)
cgConstraints = lens _cgConstraints (\ s a -> s{_cgConstraints = a});

-- | For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>.
cgGrantTokens :: Lens' CreateGrant [Text]
cgGrantTokens = lens _cgGrantTokens (\ s a -> s{_cgGrantTokens = a});

-- | List of operations permitted by the grant. This can be any combination
-- of one or more of the following values:
--
-- 1.  Decrypt
-- 2.  Encrypt
-- 3.  GenerateDataKey
-- 4.  GenerateDataKeyWithoutPlaintext
-- 5.  ReEncryptFrom
-- 6.  ReEncryptTo
-- 7.  CreateGrant
-- 8.  RetireGrant
cgOperations :: Lens' CreateGrant [GrantOperation]
cgOperations = lens _cgOperations (\ s a -> s{_cgOperations = a});

-- | A unique identifier for the customer master key. This value can be a
-- globally unique identifier or the fully specified ARN to a key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
cgKeyId :: Lens' CreateGrant Text
cgKeyId = lens _cgKeyId (\ s a -> s{_cgKeyId = a});

-- | Principal given permission by the grant to use the key identified by the
-- @keyId@ parameter.
cgGranteePrincipal :: Lens' CreateGrant Text
cgGranteePrincipal = lens _cgGranteePrincipal (\ s a -> s{_cgGranteePrincipal = a});

-- | Principal given permission to retire the grant. For more information,
-- see RetireGrant.
cgRetiringPrincipal :: Lens' CreateGrant Text
cgRetiringPrincipal = lens _cgRetiringPrincipal (\ s a -> s{_cgRetiringPrincipal = a});

instance AWSRequest CreateGrant where
        type Sv CreateGrant = KMS
        type Rs CreateGrant = CreateGrantResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateGrantResponse' <$>
                   x .:> "GrantId" <*> x .:> "GrantToken")

instance ToHeaders CreateGrant where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.CreateGrant" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateGrant where
        toJSON CreateGrant'{..}
          = object
              ["Constraints" .= _cgConstraints,
               "GrantTokens" .= _cgGrantTokens,
               "Operations" .= _cgOperations, "KeyId" .= _cgKeyId,
               "GranteePrincipal" .= _cgGranteePrincipal,
               "RetiringPrincipal" .= _cgRetiringPrincipal]

instance ToPath CreateGrant where
        toPath = const "/"

instance ToQuery CreateGrant where
        toQuery = const mempty

-- | /See:/ 'createGrantResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cgrGrantId'
--
-- * 'cgrGrantToken'
data CreateGrantResponse = CreateGrantResponse'{_cgrGrantId :: Text, _cgrGrantToken :: Text} deriving (Eq, Read, Show)

-- | 'CreateGrantResponse' smart constructor.
createGrantResponse :: Text -> Text -> CreateGrantResponse
createGrantResponse pGrantId pGrantToken = CreateGrantResponse'{_cgrGrantId = pGrantId, _cgrGrantToken = pGrantToken};

-- | Unique grant identifier. You can use the /GrantId/ value to revoke a
-- grant.
cgrGrantId :: Lens' CreateGrantResponse Text
cgrGrantId = lens _cgrGrantId (\ s a -> s{_cgrGrantId = a});

-- | For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>.
cgrGrantToken :: Lens' CreateGrantResponse Text
cgrGrantToken = lens _cgrGrantToken (\ s a -> s{_cgrGrantToken = a});
