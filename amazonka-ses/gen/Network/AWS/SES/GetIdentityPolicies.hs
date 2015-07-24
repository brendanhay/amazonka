{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetIdentityPolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the requested sending authorization policies for the given
-- identity (email address or domain). The policies are returned as a map
-- of policy names to policy contents. You can retrieve a maximum of 20
-- policies at a time.
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
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_GetIdentityPolicies.html>
module Network.AWS.SES.GetIdentityPolicies
    (
    -- * Request
      GetIdentityPolicies
    -- ** Request constructor
    , getIdentityPolicies
    -- ** Request lenses
    , gipIdentity
    , gipPolicyNames

    -- * Response
    , GetIdentityPoliciesResponse
    -- ** Response constructor
    , getIdentityPoliciesResponse
    -- ** Response lenses
    , giprsStatus
    , giprsPolicies
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types

-- | Represents a request instructing the service to retrieve the text of a
-- list of authorization policies applying to an identity.
--
-- /See:/ 'getIdentityPolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gipIdentity'
--
-- * 'gipPolicyNames'
data GetIdentityPolicies = GetIdentityPolicies'
    { _gipIdentity    :: !Text
    , _gipPolicyNames :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetIdentityPolicies' smart constructor.
getIdentityPolicies :: Text -> GetIdentityPolicies
getIdentityPolicies pIdentity_ =
    GetIdentityPolicies'
    { _gipIdentity = pIdentity_
    , _gipPolicyNames = mempty
    }

-- | The identity for which the policies will be retrieved. You can specify
-- an identity by using its name or by using its Amazon Resource Name
-- (ARN). Examples: @user\@example.com@, @example.com@,
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
--
-- To successfully call this API, you must own the identity.
gipIdentity :: Lens' GetIdentityPolicies Text
gipIdentity = lens _gipIdentity (\ s a -> s{_gipIdentity = a});

-- | A list of the names of policies to be retrieved. You can retrieve a
-- maximum of 20 policies at a time. If you do not know the names of the
-- policies that are attached to the identity, you can use
-- @ListIdentityPolicies@.
gipPolicyNames :: Lens' GetIdentityPolicies [Text]
gipPolicyNames = lens _gipPolicyNames (\ s a -> s{_gipPolicyNames = a});

instance AWSRequest GetIdentityPolicies where
        type Sv GetIdentityPolicies = SES
        type Rs GetIdentityPolicies =
             GetIdentityPoliciesResponse
        request = post "GetIdentityPolicies"
        response
          = receiveXMLWrapper "GetIdentityPoliciesResult"
              (\ s h x ->
                 GetIdentityPoliciesResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "Policies" .!@ mempty >>=
                        parseXMLMap "entry" "key" "value"))

instance ToHeaders GetIdentityPolicies where
        toHeaders = const mempty

instance ToPath GetIdentityPolicies where
        toPath = const "/"

instance ToQuery GetIdentityPolicies where
        toQuery GetIdentityPolicies'{..}
          = mconcat
              ["Action" =: ("GetIdentityPolicies" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Identity" =: _gipIdentity,
               "PolicyNames" =:
                 toQueryList "member" _gipPolicyNames]

-- | Represents a map of policy names to policies returned from a successful
-- @GetIdentityPolicies@ request.
--
-- /See:/ 'getIdentityPoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giprsStatus'
--
-- * 'giprsPolicies'
data GetIdentityPoliciesResponse = GetIdentityPoliciesResponse'
    { _giprsStatus   :: !Int
    , _giprsPolicies :: !(Map Text Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetIdentityPoliciesResponse' smart constructor.
getIdentityPoliciesResponse :: Int -> GetIdentityPoliciesResponse
getIdentityPoliciesResponse pStatus_ =
    GetIdentityPoliciesResponse'
    { _giprsStatus = pStatus_
    , _giprsPolicies = mempty
    }

-- | FIXME: Undocumented member.
giprsStatus :: Lens' GetIdentityPoliciesResponse Int
giprsStatus = lens _giprsStatus (\ s a -> s{_giprsStatus = a});

-- | A map of policy names to policies.
giprsPolicies :: Lens' GetIdentityPoliciesResponse (HashMap Text Text)
giprsPolicies = lens _giprsPolicies (\ s a -> s{_giprsPolicies = a}) . _Map;
