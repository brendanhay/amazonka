{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteIdentityPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified sending authorization policy for the given
-- identity (email address or domain). This API returns successfully even
-- if a policy with the specified name does not exist.
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
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_DeleteIdentityPolicy.html>
module Network.AWS.SES.DeleteIdentityPolicy
    (
    -- * Request
      DeleteIdentityPolicy
    -- ** Request constructor
    , deleteIdentityPolicy
    -- ** Request lenses
    , dipIdentity
    , dipPolicyName

    -- * Response
    , DeleteIdentityPolicyResponse
    -- ** Response constructor
    , deleteIdentityPolicyResponse
    -- ** Response lenses
    , diprsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types

-- | Represents a request instructing the service to delete an authorization
-- policy applying to an identity.
--
-- This request succeeds regardless of whether the specified policy exists.
--
-- /See:/ 'deleteIdentityPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dipIdentity'
--
-- * 'dipPolicyName'
data DeleteIdentityPolicy = DeleteIdentityPolicy'
    { _dipIdentity   :: !Text
    , _dipPolicyName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteIdentityPolicy' smart constructor.
deleteIdentityPolicy :: Text -> Text -> DeleteIdentityPolicy
deleteIdentityPolicy pIdentity_ pPolicyName_ =
    DeleteIdentityPolicy'
    { _dipIdentity = pIdentity_
    , _dipPolicyName = pPolicyName_
    }

-- | The identity that is associated with the policy that you want to delete.
-- You can specify the identity by using its name or by using its Amazon
-- Resource Name (ARN). Examples: @user\@example.com@, @example.com@,
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
--
-- To successfully call this API, you must own the identity.
dipIdentity :: Lens' DeleteIdentityPolicy Text
dipIdentity = lens _dipIdentity (\ s a -> s{_dipIdentity = a});

-- | The name of the policy to be deleted.
dipPolicyName :: Lens' DeleteIdentityPolicy Text
dipPolicyName = lens _dipPolicyName (\ s a -> s{_dipPolicyName = a});

instance AWSRequest DeleteIdentityPolicy where
        type Sv DeleteIdentityPolicy = SES
        type Rs DeleteIdentityPolicy =
             DeleteIdentityPolicyResponse
        request = postQuery
        response
          = receiveXMLWrapper "DeleteIdentityPolicyResult"
              (\ s h x ->
                 DeleteIdentityPolicyResponse' <$>
                   (pure (fromEnum s)))

instance ToHeaders DeleteIdentityPolicy where
        toHeaders = const mempty

instance ToPath DeleteIdentityPolicy where
        toPath = const mempty

instance ToQuery DeleteIdentityPolicy where
        toQuery DeleteIdentityPolicy'{..}
          = mconcat
              ["Action" =: ("DeleteIdentityPolicy" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Identity" =: _dipIdentity,
               "PolicyName" =: _dipPolicyName]

-- | An empty element. Receiving this element indicates that the request
-- completed successfully.
--
-- /See:/ 'deleteIdentityPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diprsStatus'
newtype DeleteIdentityPolicyResponse = DeleteIdentityPolicyResponse'
    { _diprsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteIdentityPolicyResponse' smart constructor.
deleteIdentityPolicyResponse :: Int -> DeleteIdentityPolicyResponse
deleteIdentityPolicyResponse pStatus_ =
    DeleteIdentityPolicyResponse'
    { _diprsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
diprsStatus :: Lens' DeleteIdentityPolicyResponse Int
diprsStatus = lens _diprsStatus (\ s a -> s{_diprsStatus = a});
