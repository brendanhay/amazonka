{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.KMS.RetireGrant
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

-- | Retires a grant. You can retire a grant when you\'re done using it to
-- clean up. You should revoke a grant when you intend to actively deny
-- operations that depend on it. The following are permitted to call this
-- API:
--
-- -   The account that created the grant
-- -   The @RetiringPrincipal@, if present
-- -   The @GranteePrincipal@, if @RetireGrant@ is a grantee operation
--
-- The grant to retire must be identified by its grant token or by a
-- combination of the key ARN and the grant ID. A grant token is a unique
-- variable-length base64-encoded string. A grant ID is a 64 character
-- unique identifier of a grant. Both are returned by the @CreateGrant@
-- function.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_RetireGrant.html>
module Network.AWS.KMS.RetireGrant
    (
    -- * Request
      RetireGrant
    -- ** Request constructor
    , retireGrant
    -- ** Request lenses
    , rgKeyId
    , rgGrantId
    , rgGrantToken

    -- * Response
    , RetireGrantResponse
    -- ** Response constructor
    , retireGrantResponse
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'retireGrant' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rgKeyId'
--
-- * 'rgGrantId'
--
-- * 'rgGrantToken'
data RetireGrant = RetireGrant'
    { _rgKeyId      :: !(Maybe Text)
    , _rgGrantId    :: !(Maybe Text)
    , _rgGrantToken :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'RetireGrant' smart constructor.
retireGrant :: RetireGrant
retireGrant =
    RetireGrant'
    { _rgKeyId = Nothing
    , _rgGrantId = Nothing
    , _rgGrantToken = Nothing
    }

-- | A unique identifier for the customer master key associated with the
-- grant. This value can be a globally unique identifier or a fully
-- specified ARN of the key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
rgKeyId :: Lens' RetireGrant (Maybe Text)
rgKeyId = lens _rgKeyId (\ s a -> s{_rgKeyId = a});

-- | Unique identifier of the grant to be retired. The grant ID is returned
-- by the @CreateGrant@ function.
--
-- -   Grant ID Example -
--     0123456789012345678901234567890123456789012345678901234567890123
rgGrantId :: Lens' RetireGrant (Maybe Text)
rgGrantId = lens _rgGrantId (\ s a -> s{_rgGrantId = a});

-- | Token that identifies the grant to be retired.
rgGrantToken :: Lens' RetireGrant (Maybe Text)
rgGrantToken = lens _rgGrantToken (\ s a -> s{_rgGrantToken = a});

instance AWSRequest RetireGrant where
        type Sv RetireGrant = KMS
        type Rs RetireGrant = RetireGrantResponse
        request = postJSON
        response = receiveNull RetireGrantResponse'

instance ToHeaders RetireGrant where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.RetireGrant" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RetireGrant where
        toJSON RetireGrant'{..}
          = object
              ["KeyId" .= _rgKeyId, "GrantId" .= _rgGrantId,
               "GrantToken" .= _rgGrantToken]

instance ToPath RetireGrant where
        toPath = const "/"

instance ToQuery RetireGrant where
        toQuery = const mempty

-- | /See:/ 'retireGrantResponse' smart constructor.
data RetireGrantResponse =
    RetireGrantResponse'
    deriving (Eq,Read,Show)

-- | 'RetireGrantResponse' smart constructor.
retireGrantResponse :: RetireGrantResponse
retireGrantResponse = RetireGrantResponse'
