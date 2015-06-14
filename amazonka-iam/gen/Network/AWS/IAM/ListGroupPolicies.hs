{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.ListGroupPolicies
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
-- specified group.
--
-- A group can also have managed policies attached to it. To list the
-- managed policies that are attached to a group, use
-- ListAttachedGroupPolicies. For more information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters. If there are no inline policies embedded with the specified
-- group, the action returns an empty list.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroupPolicies.html>
module Network.AWS.IAM.ListGroupPolicies
    (
    -- * Request
      ListGroupPolicies
    -- ** Request constructor
    , listGroupPolicies
    -- ** Request lenses
    , lgpGroupName
    , lgpMaxItems
    , lgpMarker

    -- * Response
    , ListGroupPoliciesResponse
    -- ** Response constructor
    , listGroupPoliciesResponse
    -- ** Response lenses
    , lgprIsTruncated
    , lgprPolicyNames
    , lgprMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'listGroupPolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgpGroupName'
--
-- * 'lgpMaxItems'
--
-- * 'lgpMarker'
data ListGroupPolicies = ListGroupPolicies'{_lgpGroupName :: Text, _lgpMaxItems :: Nat, _lgpMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListGroupPolicies' smart constructor.
listGroupPolicies :: Text -> Natural -> Text -> ListGroupPolicies
listGroupPolicies pGroupName pMaxItems pMarker = ListGroupPolicies'{_lgpGroupName = pGroupName, _lgpMaxItems = _Nat # pMaxItems, _lgpMarker = pMarker};

-- | The name of the group to list policies for.
lgpGroupName :: Lens' ListGroupPolicies Text
lgpGroupName = lens _lgpGroupName (\ s a -> s{_lgpGroupName = a});

-- | Use this only when paginating results to indicate the maximum number of
-- policy names you want in the response. If there are additional policy
-- names beyond the maximum you specify, the @IsTruncated@ response element
-- is @true@. This parameter is optional. If you do not include it, it
-- defaults to 100.
lgpMaxItems :: Lens' ListGroupPolicies Natural
lgpMaxItems = lens _lgpMaxItems (\ s a -> s{_lgpMaxItems = a}) . _Nat;

-- | Use this only when paginating results, and only in a subsequent request
-- after you\'ve received a response where the results are truncated. Set
-- it to the value of the @Marker@ element in the response you just
-- received.
lgpMarker :: Lens' ListGroupPolicies Text
lgpMarker = lens _lgpMarker (\ s a -> s{_lgpMarker = a});

instance AWSRequest ListGroupPolicies where
        type Sv ListGroupPolicies = IAM
        type Rs ListGroupPolicies = ListGroupPoliciesResponse
        request = post
        response
          = receiveXMLWrapper "ListGroupPoliciesResult"
              (\ s h x ->
                 ListGroupPoliciesResponse' <$>
                   x .@? "IsTruncated" <*>
                     (x .@? "PolicyNames" .!@ mempty >>=
                        parseXMLList "member")
                     <*> x .@ "Marker")

instance ToHeaders ListGroupPolicies where
        toHeaders = const mempty

instance ToPath ListGroupPolicies where
        toPath = const "/"

instance ToQuery ListGroupPolicies where
        toQuery ListGroupPolicies'{..}
          = mconcat
              ["Action" =: ("ListGroupPolicies" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "GroupName" =: _lgpGroupName,
               "MaxItems" =: _lgpMaxItems, "Marker" =: _lgpMarker]

-- | /See:/ 'listGroupPoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgprIsTruncated'
--
-- * 'lgprPolicyNames'
--
-- * 'lgprMarker'
data ListGroupPoliciesResponse = ListGroupPoliciesResponse'{_lgprIsTruncated :: Maybe Bool, _lgprPolicyNames :: [Text], _lgprMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListGroupPoliciesResponse' smart constructor.
listGroupPoliciesResponse :: [Text] -> Text -> ListGroupPoliciesResponse
listGroupPoliciesResponse pPolicyNames pMarker = ListGroupPoliciesResponse'{_lgprIsTruncated = Nothing, _lgprPolicyNames = pPolicyNames, _lgprMarker = pMarker};

-- | A flag that indicates whether there are more policy names to list. If
-- your results were truncated, you can make a subsequent pagination
-- request using the @Marker@ request parameter to retrieve more policy
-- names in the list.
lgprIsTruncated :: Lens' ListGroupPoliciesResponse (Maybe Bool)
lgprIsTruncated = lens _lgprIsTruncated (\ s a -> s{_lgprIsTruncated = a});

-- | A list of policy names.
lgprPolicyNames :: Lens' ListGroupPoliciesResponse [Text]
lgprPolicyNames = lens _lgprPolicyNames (\ s a -> s{_lgprPolicyNames = a});

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lgprMarker :: Lens' ListGroupPoliciesResponse Text
lgprMarker = lens _lgprMarker (\ s a -> s{_lgprMarker = a});
