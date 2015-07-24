{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.CreateGrant
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds a grant to a key to specify who can access the key and under what
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
    , cgRetiringPrincipal
    , cgConstraints
    , cgGrantTokens
    , cgOperations
    , cgKeyId
    , cgGranteePrincipal

    -- * Response
    , CreateGrantResponse
    -- ** Response constructor
    , createGrantResponse
    -- ** Response lenses
    , cgrsGrantId
    , cgrsGrantToken
    , cgrsStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createGrant' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cgRetiringPrincipal'
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
data CreateGrant = CreateGrant'
    { _cgRetiringPrincipal :: !(Maybe Text)
    , _cgConstraints       :: !(Maybe GrantConstraints)
    , _cgGrantTokens       :: !(Maybe [Text])
    , _cgOperations        :: !(Maybe [GrantOperation])
    , _cgKeyId             :: !Text
    , _cgGranteePrincipal  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateGrant' smart constructor.
createGrant :: Text -> Text -> CreateGrant
createGrant pKeyId_ pGranteePrincipal_ =
    CreateGrant'
    { _cgRetiringPrincipal = Nothing
    , _cgConstraints = Nothing
    , _cgGrantTokens = Nothing
    , _cgOperations = Nothing
    , _cgKeyId = pKeyId_
    , _cgGranteePrincipal = pGranteePrincipal_
    }

-- | Principal given permission to retire the grant. For more information,
-- see RetireGrant.
cgRetiringPrincipal :: Lens' CreateGrant (Maybe Text)
cgRetiringPrincipal = lens _cgRetiringPrincipal (\ s a -> s{_cgRetiringPrincipal = a});

-- | Specifies the conditions under which the actions specified by the
-- @Operations@ parameter are allowed.
cgConstraints :: Lens' CreateGrant (Maybe GrantConstraints)
cgConstraints = lens _cgConstraints (\ s a -> s{_cgConstraints = a});

-- | For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>.
cgGrantTokens :: Lens' CreateGrant [Text]
cgGrantTokens = lens _cgGrantTokens (\ s a -> s{_cgGrantTokens = a}) . _Default;

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
cgOperations = lens _cgOperations (\ s a -> s{_cgOperations = a}) . _Default;

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

instance AWSRequest CreateGrant where
        type Sv CreateGrant = KMS
        type Rs CreateGrant = CreateGrantResponse
        request = postJSON "CreateGrant"
        response
          = receiveJSON
              (\ s h x ->
                 CreateGrantResponse' <$>
                   (x .?> "GrantId") <*> (x .?> "GrantToken") <*>
                     (pure (fromEnum s)))

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
              ["RetiringPrincipal" .= _cgRetiringPrincipal,
               "Constraints" .= _cgConstraints,
               "GrantTokens" .= _cgGrantTokens,
               "Operations" .= _cgOperations, "KeyId" .= _cgKeyId,
               "GranteePrincipal" .= _cgGranteePrincipal]

instance ToPath CreateGrant where
        toPath = const "/"

instance ToQuery CreateGrant where
        toQuery = const mempty

-- | /See:/ 'createGrantResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cgrsGrantId'
--
-- * 'cgrsGrantToken'
--
-- * 'cgrsStatus'
data CreateGrantResponse = CreateGrantResponse'
    { _cgrsGrantId    :: !(Maybe Text)
    , _cgrsGrantToken :: !(Maybe Text)
    , _cgrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateGrantResponse' smart constructor.
createGrantResponse :: Int -> CreateGrantResponse
createGrantResponse pStatus_ =
    CreateGrantResponse'
    { _cgrsGrantId = Nothing
    , _cgrsGrantToken = Nothing
    , _cgrsStatus = pStatus_
    }

-- | Unique grant identifier. You can use the /GrantId/ value to revoke a
-- grant.
cgrsGrantId :: Lens' CreateGrantResponse (Maybe Text)
cgrsGrantId = lens _cgrsGrantId (\ s a -> s{_cgrsGrantId = a});

-- | For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>.
cgrsGrantToken :: Lens' CreateGrantResponse (Maybe Text)
cgrsGrantToken = lens _cgrsGrantToken (\ s a -> s{_cgrsGrantToken = a});

-- | FIXME: Undocumented member.
cgrsStatus :: Lens' CreateGrantResponse Int
cgrsStatus = lens _cgrsStatus (\ s a -> s{_cgrsStatus = a});
