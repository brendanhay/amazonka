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
-- Module      : Network.AWS.KMS.GetKeyPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a policy attached to the specified key.
--
-- /See:/ <http://docs.aws.amazon.com/kms/latest/APIReference/API_GetKeyPolicy.html AWS API Reference> for GetKeyPolicy.
module Network.AWS.KMS.GetKeyPolicy
    (
    -- * Creating a Request
      getKeyPolicy
    , GetKeyPolicy
    -- * Request Lenses
    , gkpKeyId
    , gkpPolicyName

    -- * Destructuring the Response
    , getKeyPolicyResponse
    , GetKeyPolicyResponse
    -- * Response Lenses
    , gkprsPolicy
    , gkprsStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.KMS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getKeyPolicy' smart constructor.
data GetKeyPolicy = GetKeyPolicy'
    { _gkpKeyId      :: !Text
    , _gkpPolicyName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetKeyPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gkpKeyId'
--
-- * 'gkpPolicyName'
getKeyPolicy
    :: Text -- ^ 'gkpKeyId'
    -> Text -- ^ 'gkpPolicyName'
    -> GetKeyPolicy
getKeyPolicy pKeyId_ pPolicyName_ =
    GetKeyPolicy'
    { _gkpKeyId = pKeyId_
    , _gkpPolicyName = pPolicyName_
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
        type Rs GetKeyPolicy = GetKeyPolicyResponse
        request = postJSON kMS
        response
          = receiveJSON
              (\ s h x ->
                 GetKeyPolicyResponse' <$>
                   (x .?> "Policy") <*> (pure (fromEnum s)))

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
              (catMaybes
                 [Just ("KeyId" .= _gkpKeyId),
                  Just ("PolicyName" .= _gkpPolicyName)])

instance ToPath GetKeyPolicy where
        toPath = const "/"

instance ToQuery GetKeyPolicy where
        toQuery = const mempty

-- | /See:/ 'getKeyPolicyResponse' smart constructor.
data GetKeyPolicyResponse = GetKeyPolicyResponse'
    { _gkprsPolicy :: !(Maybe Text)
    , _gkprsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetKeyPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gkprsPolicy'
--
-- * 'gkprsStatus'
getKeyPolicyResponse
    :: Int -- ^ 'gkprsStatus'
    -> GetKeyPolicyResponse
getKeyPolicyResponse pStatus_ =
    GetKeyPolicyResponse'
    { _gkprsPolicy = Nothing
    , _gkprsStatus = pStatus_
    }

-- | A policy document in JSON format.
gkprsPolicy :: Lens' GetKeyPolicyResponse (Maybe Text)
gkprsPolicy = lens _gkprsPolicy (\ s a -> s{_gkprsPolicy = a});

-- | The response status code.
gkprsStatus :: Lens' GetKeyPolicyResponse Int
gkprsStatus = lens _gkprsStatus (\ s a -> s{_gkprsStatus = a});
