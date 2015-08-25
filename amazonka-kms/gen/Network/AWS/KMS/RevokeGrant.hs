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
-- Module      : Network.AWS.KMS.RevokeGrant
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes a grant. You can revoke a grant to actively deny operations that
-- depend on it.
--
-- /See:/ <http://docs.aws.amazon.com/kms/latest/APIReference/API_RevokeGrant.html AWS API Reference> for RevokeGrant.
module Network.AWS.KMS.RevokeGrant
    (
    -- * Creating a Request
      revokeGrant
    , RevokeGrant
    -- * Request Lenses
    , rKeyId
    , rGrantId

    -- * Destructuring the Response
    , revokeGrantResponse
    , RevokeGrantResponse
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.KMS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'revokeGrant' smart constructor.
data RevokeGrant = RevokeGrant'
    { _rKeyId   :: !Text
    , _rGrantId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RevokeGrant' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rKeyId'
--
-- * 'rGrantId'
revokeGrant
    :: Text -- ^ 'rKeyId'
    -> Text -- ^ 'rGrantId'
    -> RevokeGrant
revokeGrant pKeyId_ pGrantId_ =
    RevokeGrant'
    { _rKeyId = pKeyId_
    , _rGrantId = pGrantId_
    }

-- | A unique identifier for the customer master key associated with the
-- grant. This value can be a globally unique identifier or the fully
-- specified ARN to a key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
rKeyId :: Lens' RevokeGrant Text
rKeyId = lens _rKeyId (\ s a -> s{_rKeyId = a});

-- | Identifier of the grant to be revoked.
rGrantId :: Lens' RevokeGrant Text
rGrantId = lens _rGrantId (\ s a -> s{_rGrantId = a});

instance AWSRequest RevokeGrant where
        type Rs RevokeGrant = RevokeGrantResponse
        request = postJSON kMS
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
              (catMaybes
                 [Just ("KeyId" .= _rKeyId),
                  Just ("GrantId" .= _rGrantId)])

instance ToPath RevokeGrant where
        toPath = const "/"

instance ToQuery RevokeGrant where
        toQuery = const mempty

-- | /See:/ 'revokeGrantResponse' smart constructor.
data RevokeGrantResponse =
    RevokeGrantResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RevokeGrantResponse' with the minimum fields required to make a request.
--
revokeGrantResponse
    :: RevokeGrantResponse
revokeGrantResponse = RevokeGrantResponse'
