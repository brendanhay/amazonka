{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.RetireGrant
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retires a grant. You can retire a grant when you\'re done using it to
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
    , rgrqKeyId
    , rgrqGrantId
    , rgrqGrantToken

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
-- * 'rgrqKeyId'
--
-- * 'rgrqGrantId'
--
-- * 'rgrqGrantToken'
data RetireGrant = RetireGrant'
    { _rgrqKeyId      :: !(Maybe Text)
    , _rgrqGrantId    :: !(Maybe Text)
    , _rgrqGrantToken :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RetireGrant' smart constructor.
retireGrant :: RetireGrant
retireGrant =
    RetireGrant'
    { _rgrqKeyId = Nothing
    , _rgrqGrantId = Nothing
    , _rgrqGrantToken = Nothing
    }

-- | A unique identifier for the customer master key associated with the
-- grant. This value can be a globally unique identifier or a fully
-- specified ARN of the key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
rgrqKeyId :: Lens' RetireGrant (Maybe Text)
rgrqKeyId = lens _rgrqKeyId (\ s a -> s{_rgrqKeyId = a});

-- | Unique identifier of the grant to be retired. The grant ID is returned
-- by the @CreateGrant@ function.
--
-- -   Grant ID Example -
--     0123456789012345678901234567890123456789012345678901234567890123
rgrqGrantId :: Lens' RetireGrant (Maybe Text)
rgrqGrantId = lens _rgrqGrantId (\ s a -> s{_rgrqGrantId = a});

-- | Token that identifies the grant to be retired.
rgrqGrantToken :: Lens' RetireGrant (Maybe Text)
rgrqGrantToken = lens _rgrqGrantToken (\ s a -> s{_rgrqGrantToken = a});

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
              ["KeyId" .= _rgrqKeyId, "GrantId" .= _rgrqGrantId,
               "GrantToken" .= _rgrqGrantToken]

instance ToPath RetireGrant where
        toPath = const "/"

instance ToQuery RetireGrant where
        toQuery = const mempty

-- | /See:/ 'retireGrantResponse' smart constructor.
data RetireGrantResponse =
    RetireGrantResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RetireGrantResponse' smart constructor.
retireGrantResponse :: RetireGrantResponse
retireGrantResponse = RetireGrantResponse'
