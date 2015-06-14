{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.ListRolePolicies
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

-- | Lists the names of the inline policies that are embedded in the
-- specified role.
--
-- A role can also have managed policies attached to it. To list the
-- managed policies that are attached to a role, use
-- ListAttachedRolePolicies. For more information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters. If there are no inline policies embedded with the specified
-- role, the action returns an empty list.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListRolePolicies.html>
module Network.AWS.IAM.ListRolePolicies
    (
    -- * Request
      ListRolePolicies
    -- ** Request constructor
    , listRolePolicies
    -- ** Request lenses
    , lrpRoleName
    , lrpMaxItems
    , lrpMarker

    -- * Response
    , ListRolePoliciesResponse
    -- ** Response constructor
    , listRolePoliciesResponse
    -- ** Response lenses
    , lrprIsTruncated
    , lrprPolicyNames
    , lrprMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'listRolePolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrpRoleName'
--
-- * 'lrpMaxItems'
--
-- * 'lrpMarker'
data ListRolePolicies = ListRolePolicies'{_lrpRoleName :: Text, _lrpMaxItems :: Nat, _lrpMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListRolePolicies' smart constructor.
listRolePolicies :: Text -> Natural -> Text -> ListRolePolicies
listRolePolicies pRoleName pMaxItems pMarker = ListRolePolicies'{_lrpRoleName = pRoleName, _lrpMaxItems = _Nat # pMaxItems, _lrpMarker = pMarker};

-- | The name of the role to list policies for.
lrpRoleName :: Lens' ListRolePolicies Text
lrpRoleName = lens _lrpRoleName (\ s a -> s{_lrpRoleName = a});

-- | Use this parameter only when paginating results to indicate the maximum
-- number of role policies you want in the response. If there are
-- additional role policies beyond the maximum you specify, the
-- @IsTruncated@ response element is @true@. This parameter is optional. If
-- you do not include it, it defaults to 100.
lrpMaxItems :: Lens' ListRolePolicies Natural
lrpMaxItems = lens _lrpMaxItems (\ s a -> s{_lrpMaxItems = a}) . _Nat;

-- | Use this parameter only when paginating results, and only in a
-- subsequent request after you\'ve received a response where the results
-- are truncated. Set it to the value of the @Marker@ element in the
-- response you just received.
lrpMarker :: Lens' ListRolePolicies Text
lrpMarker = lens _lrpMarker (\ s a -> s{_lrpMarker = a});

instance AWSRequest ListRolePolicies where
        type Sv ListRolePolicies = IAM
        type Rs ListRolePolicies = ListRolePoliciesResponse
        request = post
        response
          = receiveXMLWrapper "ListRolePoliciesResult"
              (\ s h x ->
                 ListRolePoliciesResponse' <$>
                   x .@? "IsTruncated" <*>
                     (x .@? "PolicyNames" .!@ mempty >>=
                        parseXMLList "member")
                     <*> x .@ "Marker")

instance ToHeaders ListRolePolicies where
        toHeaders = const mempty

instance ToPath ListRolePolicies where
        toPath = const "/"

instance ToQuery ListRolePolicies where
        toQuery ListRolePolicies'{..}
          = mconcat
              ["Action" =: ("ListRolePolicies" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RoleName" =: _lrpRoleName,
               "MaxItems" =: _lrpMaxItems, "Marker" =: _lrpMarker]

-- | /See:/ 'listRolePoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrprIsTruncated'
--
-- * 'lrprPolicyNames'
--
-- * 'lrprMarker'
data ListRolePoliciesResponse = ListRolePoliciesResponse'{_lrprIsTruncated :: Maybe Bool, _lrprPolicyNames :: [Text], _lrprMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListRolePoliciesResponse' smart constructor.
listRolePoliciesResponse :: [Text] -> Text -> ListRolePoliciesResponse
listRolePoliciesResponse pPolicyNames pMarker = ListRolePoliciesResponse'{_lrprIsTruncated = Nothing, _lrprPolicyNames = pPolicyNames, _lrprMarker = pMarker};

-- | A flag that indicates whether there are more policy names to list. If
-- your results were truncated, you can make a subsequent pagination
-- request using the @Marker@ request parameter to retrieve more policy
-- names in the list.
lrprIsTruncated :: Lens' ListRolePoliciesResponse (Maybe Bool)
lrprIsTruncated = lens _lrprIsTruncated (\ s a -> s{_lrprIsTruncated = a});

-- | A list of policy names.
lrprPolicyNames :: Lens' ListRolePoliciesResponse [Text]
lrprPolicyNames = lens _lrprPolicyNames (\ s a -> s{_lrprPolicyNames = a});

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lrprMarker :: Lens' ListRolePoliciesResponse Text
lrprMarker = lens _lrprMarker (\ s a -> s{_lrprMarker = a});
