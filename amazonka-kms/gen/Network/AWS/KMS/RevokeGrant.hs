{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.KMS.RevokeGrant
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Revokes a grant. You can revoke a grant to actively deny operations that
-- depend on it.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_RevokeGrant.html>
module Network.AWS.KMS.RevokeGrant
    (
    -- * Request
      RevokeGrant
    -- ** Request constructor
    , revokeGrant
    -- ** Request lenses
    , revKeyId
    , revGrantId

    -- * Response
    , RevokeGrantResponse
    -- ** Response constructor
    , revokeGrantResponse
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'revokeGrant' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'revKeyId'
--
-- * 'revGrantId'
data RevokeGrant = RevokeGrant'
    { _revKeyId   :: !Text
    , _revGrantId :: !Text
    } deriving (Eq,Read,Show)

-- | 'RevokeGrant' smart constructor.
revokeGrant :: Text -> Text -> RevokeGrant
revokeGrant pKeyId pGrantId =
    RevokeGrant'
    { _revKeyId = pKeyId
    , _revGrantId = pGrantId
    }

-- | A unique identifier for the customer master key associated with the
-- grant. This value can be a globally unique identifier or the fully
-- specified ARN to a key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
revKeyId :: Lens' RevokeGrant Text
revKeyId = lens _revKeyId (\ s a -> s{_revKeyId = a});

-- | Identifier of the grant to be revoked.
revGrantId :: Lens' RevokeGrant Text
revGrantId = lens _revGrantId (\ s a -> s{_revGrantId = a});

instance AWSRequest RevokeGrant where
        type Sv RevokeGrant = KMS
        type Rs RevokeGrant = RevokeGrantResponse
        request = postJSON
        response = receiveNull RevokeGrantResponse'

instance ToHeaders RevokeGrant where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.RevokeGrant" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RevokeGrant where
        toJSON RevokeGrant'{..}
          = object
              ["KeyId" .= _revKeyId, "GrantId" .= _revGrantId]

instance ToPath RevokeGrant where
        toPath = const "/"

instance ToQuery RevokeGrant where
        toQuery = const mempty

-- | /See:/ 'revokeGrantResponse' smart constructor.
data RevokeGrantResponse =
    RevokeGrantResponse'
    deriving (Eq,Read,Show)

-- | 'RevokeGrantResponse' smart constructor.
revokeGrantResponse :: RevokeGrantResponse
revokeGrantResponse = RevokeGrantResponse'
