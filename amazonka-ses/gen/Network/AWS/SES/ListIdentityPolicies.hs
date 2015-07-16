{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ListIdentityPolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of sending authorization policies that are attached to
-- the given identity (email address or domain). This API returns only a
-- list. If you want the actual policy content, you can use
-- @GetIdentityPolicies@.
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
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_ListIdentityPolicies.html>
module Network.AWS.SES.ListIdentityPolicies
    (
    -- * Request
      ListIdentityPolicies
    -- ** Request constructor
    , listIdentityPolicies
    -- ** Request lenses
    , lipIdentity

    -- * Response
    , ListIdentityPoliciesResponse
    -- ** Response constructor
    , listIdentityPoliciesResponse
    -- ** Response lenses
    , liprStatus
    , liprPolicyNames
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types

-- | Represents a request instructing the service to list all authorization
-- policies, by name, applying to an identity.
--
-- /See:/ 'listIdentityPolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lipIdentity'
newtype ListIdentityPolicies = ListIdentityPolicies'
    { _lipIdentity :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListIdentityPolicies' smart constructor.
listIdentityPolicies :: Text -> ListIdentityPolicies
listIdentityPolicies pIdentity =
    ListIdentityPolicies'
    { _lipIdentity = pIdentity
    }

-- | The identity that is associated with the policy for which the policies
-- will be listed. You can specify an identity by using its name or by
-- using its Amazon Resource Name (ARN). Examples: @user\@example.com@,
-- @example.com@,
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
--
-- To successfully call this API, you must own the identity.
lipIdentity :: Lens' ListIdentityPolicies Text
lipIdentity = lens _lipIdentity (\ s a -> s{_lipIdentity = a});

instance AWSRequest ListIdentityPolicies where
        type Sv ListIdentityPolicies = SES
        type Rs ListIdentityPolicies =
             ListIdentityPoliciesResponse
        request = post
        response
          = receiveXMLWrapper "ListIdentityPoliciesResult"
              (\ s h x ->
                 ListIdentityPoliciesResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "PolicyNames" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListIdentityPolicies where
        toHeaders = const mempty

instance ToPath ListIdentityPolicies where
        toPath = const "/"

instance ToQuery ListIdentityPolicies where
        toQuery ListIdentityPolicies'{..}
          = mconcat
              ["Action" =: ("ListIdentityPolicies" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Identity" =: _lipIdentity]

-- | Represents a list of policy names returned from a successful
-- @ListIdentityPolicies@ request.
--
-- /See:/ 'listIdentityPoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'liprStatus'
--
-- * 'liprPolicyNames'
data ListIdentityPoliciesResponse = ListIdentityPoliciesResponse'
    { _liprStatus      :: !Int
    , _liprPolicyNames :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListIdentityPoliciesResponse' smart constructor.
listIdentityPoliciesResponse :: Int -> ListIdentityPoliciesResponse
listIdentityPoliciesResponse pStatus =
    ListIdentityPoliciesResponse'
    { _liprStatus = pStatus
    , _liprPolicyNames = mempty
    }

-- | FIXME: Undocumented member.
liprStatus :: Lens' ListIdentityPoliciesResponse Int
liprStatus = lens _liprStatus (\ s a -> s{_liprStatus = a});

-- | A list of names of policies that apply to the specified identity.
liprPolicyNames :: Lens' ListIdentityPoliciesResponse [Text]
liprPolicyNames = lens _liprPolicyNames (\ s a -> s{_liprPolicyNames = a});
