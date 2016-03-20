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
-- Module      : Network.AWS.SES.PutIdentityPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates a sending authorization policy for the specified
-- identity (email address or domain).
--
-- This API is for the identity owner only. If you have not verified the
-- identity, this API will return an error.
--
-- Sending authorization is a feature that enables an identity owner to
-- authorize other senders to use its identities. For information about
-- using sending authorization, see the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
--
-- This action is throttled at one request per second.
module Network.AWS.SES.PutIdentityPolicy
    (
    -- * Creating a Request
      putIdentityPolicy
    , PutIdentityPolicy
    -- * Request Lenses
    , pipIdentity
    , pipPolicyName
    , pipPolicy

    -- * Destructuring the Response
    , putIdentityPolicyResponse
    , PutIdentityPolicyResponse
    -- * Response Lenses
    , piprsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | /See:/ 'putIdentityPolicy' smart constructor.
data PutIdentityPolicy = PutIdentityPolicy'
    { _pipIdentity   :: !Text
    , _pipPolicyName :: !Text
    , _pipPolicy     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutIdentityPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pipIdentity'
--
-- * 'pipPolicyName'
--
-- * 'pipPolicy'
putIdentityPolicy
    :: Text -- ^ 'pipIdentity'
    -> Text -- ^ 'pipPolicyName'
    -> Text -- ^ 'pipPolicy'
    -> PutIdentityPolicy
putIdentityPolicy pIdentity_ pPolicyName_ pPolicy_ =
    PutIdentityPolicy'
    { _pipIdentity = pIdentity_
    , _pipPolicyName = pPolicyName_
    , _pipPolicy = pPolicy_
    }

-- | The identity to which the policy will apply. You can specify an identity
-- by using its name or by using its Amazon Resource Name (ARN). Examples:
-- 'user\'example.com', 'example.com',
-- 'arn:aws:ses:us-east-1:123456789012:identity\/example.com'.
--
-- To successfully call this API, you must own the identity.
pipIdentity :: Lens' PutIdentityPolicy Text
pipIdentity = lens _pipIdentity (\ s a -> s{_pipIdentity = a});

-- | The name of the policy.
--
-- The policy name cannot exceed 64 characters and can only include
-- alphanumeric characters, dashes, and underscores.
pipPolicyName :: Lens' PutIdentityPolicy Text
pipPolicyName = lens _pipPolicyName (\ s a -> s{_pipPolicyName = a});

-- | The text of the policy in JSON format. The policy cannot exceed 4 KB.
--
-- For information about the syntax of sending authorization policies, see
-- the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-policies.html Amazon SES Developer Guide>.
pipPolicy :: Lens' PutIdentityPolicy Text
pipPolicy = lens _pipPolicy (\ s a -> s{_pipPolicy = a});

instance AWSRequest PutIdentityPolicy where
        type Rs PutIdentityPolicy = PutIdentityPolicyResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "PutIdentityPolicyResult"
              (\ s h x ->
                 PutIdentityPolicyResponse' <$> (pure (fromEnum s)))

instance Hashable PutIdentityPolicy

instance ToHeaders PutIdentityPolicy where
        toHeaders = const mempty

instance ToPath PutIdentityPolicy where
        toPath = const "/"

instance ToQuery PutIdentityPolicy where
        toQuery PutIdentityPolicy'{..}
          = mconcat
              ["Action" =: ("PutIdentityPolicy" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Identity" =: _pipIdentity,
               "PolicyName" =: _pipPolicyName,
               "Policy" =: _pipPolicy]

-- | /See:/ 'putIdentityPolicyResponse' smart constructor.
newtype PutIdentityPolicyResponse = PutIdentityPolicyResponse'
    { _piprsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutIdentityPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piprsResponseStatus'
putIdentityPolicyResponse
    :: Int -- ^ 'piprsResponseStatus'
    -> PutIdentityPolicyResponse
putIdentityPolicyResponse pResponseStatus_ =
    PutIdentityPolicyResponse'
    { _piprsResponseStatus = pResponseStatus_
    }

-- | The response status code.
piprsResponseStatus :: Lens' PutIdentityPolicyResponse Int
piprsResponseStatus = lens _piprsResponseStatus (\ s a -> s{_piprsResponseStatus = a});
