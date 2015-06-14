{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.ListPolicies
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

-- | Lists all the managed policies that are available to your account,
-- including your own customer managed policies and all AWS managed
-- policies.
--
-- You can filter the list of policies that is returned using the optional
-- @OnlyAttached@, @Scope@, and @PathPrefix@ parameters. For example, to
-- list only the customer managed policies in your AWS account, set @Scope@
-- to @Local@. To list only AWS managed policies, set @Scope@ to @AWS@.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- For more information about managed policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListPolicies.html>
module Network.AWS.IAM.ListPolicies
    (
    -- * Request
      ListPolicies
    -- ** Request constructor
    , listPolicies
    -- ** Request lenses
    , lpPathPrefix
    , lpOnlyAttached
    , lpScope
    , lpMaxItems
    , lpMarker

    -- * Response
    , ListPoliciesResponse
    -- ** Response constructor
    , listPoliciesResponse
    -- ** Response lenses
    , lprIsTruncated
    , lprPolicies
    , lprMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'listPolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpPathPrefix'
--
-- * 'lpOnlyAttached'
--
-- * 'lpScope'
--
-- * 'lpMaxItems'
--
-- * 'lpMarker'
data ListPolicies = ListPolicies'{_lpPathPrefix :: Maybe Text, _lpOnlyAttached :: Maybe Bool, _lpScope :: Maybe PolicyScopeType, _lpMaxItems :: Nat, _lpMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListPolicies' smart constructor.
listPolicies :: Natural -> Text -> ListPolicies
listPolicies pMaxItems pMarker = ListPolicies'{_lpPathPrefix = Nothing, _lpOnlyAttached = Nothing, _lpScope = Nothing, _lpMaxItems = _Nat # pMaxItems, _lpMarker = pMarker};

-- | The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- policies.
lpPathPrefix :: Lens' ListPolicies (Maybe Text)
lpPathPrefix = lens _lpPathPrefix (\ s a -> s{_lpPathPrefix = a});

-- | A flag to filter the results to only the attached policies.
--
-- When @OnlyAttached@ is @true@, the returned list contains only the
-- policies that are attached to a user, group, or role. When
-- @OnlyAttached@ is @false@, or when the parameter is not included, all
-- policies are returned.
lpOnlyAttached :: Lens' ListPolicies (Maybe Bool)
lpOnlyAttached = lens _lpOnlyAttached (\ s a -> s{_lpOnlyAttached = a});

-- | The scope to use for filtering the results.
--
-- To list only AWS managed policies, set @Scope@ to @AWS@. To list only
-- the customer managed policies in your AWS account, set @Scope@ to
-- @Local@.
--
-- This parameter is optional. If it is not included, or if it is set to
-- @All@, all policies are returned.
lpScope :: Lens' ListPolicies (Maybe PolicyScopeType)
lpScope = lens _lpScope (\ s a -> s{_lpScope = a});

-- | Use this parameter only when paginating results to indicate the maximum
-- number of policies you want in the response. If there are additional
-- policies beyond the maximum you specify, the @IsTruncated@ response
-- element is @true@. This parameter is optional. If you do not include it,
-- it defaults to 100.
lpMaxItems :: Lens' ListPolicies Natural
lpMaxItems = lens _lpMaxItems (\ s a -> s{_lpMaxItems = a}) . _Nat;

-- | Use this parameter only when paginating results, and only in a
-- subsequent request after you\'ve received a response where the results
-- are truncated. Set it to the value of the @Marker@ element in the
-- response you just received.
lpMarker :: Lens' ListPolicies Text
lpMarker = lens _lpMarker (\ s a -> s{_lpMarker = a});

instance AWSRequest ListPolicies where
        type Sv ListPolicies = IAM
        type Rs ListPolicies = ListPoliciesResponse
        request = post
        response
          = receiveXMLWrapper "ListPoliciesResult"
              (\ s h x ->
                 ListPoliciesResponse' <$>
                   x .@? "IsTruncated" <*>
                     (x .@? "Policies" .!@ mempty >>=
                        parseXMLList "member")
                     <*> x .@ "Marker")

instance ToHeaders ListPolicies where
        toHeaders = const mempty

instance ToPath ListPolicies where
        toPath = const "/"

instance ToQuery ListPolicies where
        toQuery ListPolicies'{..}
          = mconcat
              ["Action" =: ("ListPolicies" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _lpPathPrefix,
               "OnlyAttached" =: _lpOnlyAttached,
               "Scope" =: _lpScope, "MaxItems" =: _lpMaxItems,
               "Marker" =: _lpMarker]

-- | /See:/ 'listPoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lprIsTruncated'
--
-- * 'lprPolicies'
--
-- * 'lprMarker'
data ListPoliciesResponse = ListPoliciesResponse'{_lprIsTruncated :: Maybe Bool, _lprPolicies :: [Policy], _lprMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListPoliciesResponse' smart constructor.
listPoliciesResponse :: Text -> ListPoliciesResponse
listPoliciesResponse pMarker = ListPoliciesResponse'{_lprIsTruncated = Nothing, _lprPolicies = mempty, _lprMarker = pMarker};

-- | A flag that indicates whether there are more policies to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more policies in the
-- list.
lprIsTruncated :: Lens' ListPoliciesResponse (Maybe Bool)
lprIsTruncated = lens _lprIsTruncated (\ s a -> s{_lprIsTruncated = a});

-- | A list of policies.
lprPolicies :: Lens' ListPoliciesResponse [Policy]
lprPolicies = lens _lprPolicies (\ s a -> s{_lprPolicies = a});

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lprMarker :: Lens' ListPoliciesResponse Text
lprMarker = lens _lprMarker (\ s a -> s{_lprMarker = a});
