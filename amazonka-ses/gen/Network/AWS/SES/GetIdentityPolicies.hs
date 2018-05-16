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
-- Module      : Network.AWS.SES.GetIdentityPolicies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the requested sending authorization policies for the given identity (an email address or a domain). The policies are returned as a map of policy names to policy contents. You can retrieve a maximum of 20 policies at a time.
--
--
-- Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.GetIdentityPolicies
    (
    -- * Creating a Request
      getIdentityPolicies
    , GetIdentityPolicies
    -- * Request Lenses
    , gipIdentity
    , gipPolicyNames

    -- * Destructuring the Response
    , getIdentityPoliciesResponse
    , GetIdentityPoliciesResponse
    -- * Response Lenses
    , giprsResponseStatus
    , giprsPolicies
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to return the requested sending authorization policies for an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'getIdentityPolicies' smart constructor.
data GetIdentityPolicies = GetIdentityPolicies'
  { _gipIdentity    :: !Text
  , _gipPolicyNames :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIdentityPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gipIdentity' - The identity for which the policies will be retrieved. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ . To successfully call this API, you must own the identity.
--
-- * 'gipPolicyNames' - A list of the names of policies to be retrieved. You can retrieve a maximum of 20 policies at a time. If you do not know the names of the policies that are attached to the identity, you can use @ListIdentityPolicies@ .
getIdentityPolicies
    :: Text -- ^ 'gipIdentity'
    -> GetIdentityPolicies
getIdentityPolicies pIdentity_ =
  GetIdentityPolicies' {_gipIdentity = pIdentity_, _gipPolicyNames = mempty}


-- | The identity for which the policies will be retrieved. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ . To successfully call this API, you must own the identity.
gipIdentity :: Lens' GetIdentityPolicies Text
gipIdentity = lens _gipIdentity (\ s a -> s{_gipIdentity = a})

-- | A list of the names of policies to be retrieved. You can retrieve a maximum of 20 policies at a time. If you do not know the names of the policies that are attached to the identity, you can use @ListIdentityPolicies@ .
gipPolicyNames :: Lens' GetIdentityPolicies [Text]
gipPolicyNames = lens _gipPolicyNames (\ s a -> s{_gipPolicyNames = a}) . _Coerce

instance AWSRequest GetIdentityPolicies where
        type Rs GetIdentityPolicies =
             GetIdentityPoliciesResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "GetIdentityPoliciesResult"
              (\ s h x ->
                 GetIdentityPoliciesResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "Policies" .!@ mempty >>=
                        parseXMLMap "entry" "key" "value"))

instance Hashable GetIdentityPolicies where

instance NFData GetIdentityPolicies where

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

-- | Represents the requested sending authorization policies.
--
--
--
-- /See:/ 'getIdentityPoliciesResponse' smart constructor.
data GetIdentityPoliciesResponse = GetIdentityPoliciesResponse'
  { _giprsResponseStatus :: !Int
  , _giprsPolicies       :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIdentityPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giprsResponseStatus' - -- | The response status code.
--
-- * 'giprsPolicies' - A map of policy names to policies.
getIdentityPoliciesResponse
    :: Int -- ^ 'giprsResponseStatus'
    -> GetIdentityPoliciesResponse
getIdentityPoliciesResponse pResponseStatus_ =
  GetIdentityPoliciesResponse'
    {_giprsResponseStatus = pResponseStatus_, _giprsPolicies = mempty}


-- | -- | The response status code.
giprsResponseStatus :: Lens' GetIdentityPoliciesResponse Int
giprsResponseStatus = lens _giprsResponseStatus (\ s a -> s{_giprsResponseStatus = a})

-- | A map of policy names to policies.
giprsPolicies :: Lens' GetIdentityPoliciesResponse (HashMap Text Text)
giprsPolicies = lens _giprsPolicies (\ s a -> s{_giprsPolicies = a}) . _Map

instance NFData GetIdentityPoliciesResponse where
