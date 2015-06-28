{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.KMS.GetKeyPolicy
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

-- | Retrieves a policy attached to the specified key.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_GetKeyPolicy.html>
module Network.AWS.KMS.GetKeyPolicy
    (
    -- * Request
      GetKeyPolicy
    -- ** Request constructor
    , getKeyPolicy
    -- ** Request lenses
    , gkpKeyId
    , gkpPolicyName

    -- * Response
    , GetKeyPolicyResponse
    -- ** Response constructor
    , getKeyPolicyResponse
    -- ** Response lenses
    , gkprPolicy
    , gkprStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getKeyPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gkpKeyId'
--
-- * 'gkpPolicyName'
data GetKeyPolicy = GetKeyPolicy'
    { _gkpKeyId      :: !Text
    , _gkpPolicyName :: !Text
    } deriving (Eq,Read,Show)

-- | 'GetKeyPolicy' smart constructor.
getKeyPolicy :: Text -> Text -> GetKeyPolicy
getKeyPolicy pKeyId pPolicyName =
    GetKeyPolicy'
    { _gkpKeyId = pKeyId
    , _gkpPolicyName = pPolicyName
    }

-- | A unique identifier for the customer master key. This value can be a
-- globally unique identifier or the fully specified ARN to a key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
gkpKeyId :: Lens' GetKeyPolicy Text
gkpKeyId = lens _gkpKeyId (\ s a -> s{_gkpKeyId = a});

-- | String that contains the name of the policy. Currently, this must be
-- \"default\". Policy names can be discovered by calling ListKeyPolicies.
gkpPolicyName :: Lens' GetKeyPolicy Text
gkpPolicyName = lens _gkpPolicyName (\ s a -> s{_gkpPolicyName = a});

instance AWSRequest GetKeyPolicy where
        type Sv GetKeyPolicy = KMS
        type Rs GetKeyPolicy = GetKeyPolicyResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetKeyPolicyResponse' <$>
                   (x .?> "Policy") <*> (pure s))

instance ToHeaders GetKeyPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.GetKeyPolicy" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetKeyPolicy where
        toJSON GetKeyPolicy'{..}
          = object
              ["KeyId" .= _gkpKeyId,
               "PolicyName" .= _gkpPolicyName]

instance ToPath GetKeyPolicy where
        toPath = const "/"

instance ToQuery GetKeyPolicy where
        toQuery = const mempty

-- | /See:/ 'getKeyPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gkprPolicy'
--
-- * 'gkprStatus'
data GetKeyPolicyResponse = GetKeyPolicyResponse'
    { _gkprPolicy :: !(Maybe Text)
    , _gkprStatus :: !Status
    } deriving (Eq,Show)

-- | 'GetKeyPolicyResponse' smart constructor.
getKeyPolicyResponse :: Status -> GetKeyPolicyResponse
getKeyPolicyResponse pStatus =
    GetKeyPolicyResponse'
    { _gkprPolicy = Nothing
    , _gkprStatus = pStatus
    }

-- | A policy document in JSON format.
gkprPolicy :: Lens' GetKeyPolicyResponse (Maybe Text)
gkprPolicy = lens _gkprPolicy (\ s a -> s{_gkprPolicy = a});

-- | FIXME: Undocumented member.
gkprStatus :: Lens' GetKeyPolicyResponse Status
gkprStatus = lens _gkprStatus (\ s a -> s{_gkprStatus = a});
