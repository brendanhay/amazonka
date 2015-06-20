{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.ListAttachedRolePolicies
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

-- | Lists all managed policies that are attached to the specified role.
--
-- A role can also have inline policies embedded with it. To list the
-- inline policies for a role, use the ListRolePolicies API. For
-- information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters. You can use the @PathPrefix@ parameter to limit the list of
-- policies to only those matching the specified path prefix. If there are
-- no policies attached to the specified role (or none that match the
-- specified path prefix), the action returns an empty list.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAttachedRolePolicies.html>
module Network.AWS.IAM.ListAttachedRolePolicies
    (
    -- * Request
      ListAttachedRolePolicies
    -- ** Request constructor
    , listAttachedRolePolicies
    -- ** Request lenses
    , larpPathPrefix
    , larpMaxItems
    , larpMarker
    , larpRoleName

    -- * Response
    , ListAttachedRolePoliciesResponse
    -- ** Response constructor
    , listAttachedRolePoliciesResponse
    -- ** Response lenses
    , larprAttachedPolicies
    , larprMarker
    , larprIsTruncated
    ) where

import Network.AWS.IAM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAttachedRolePolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'larpPathPrefix'
--
-- * 'larpMaxItems'
--
-- * 'larpMarker'
--
-- * 'larpRoleName'
data ListAttachedRolePolicies = ListAttachedRolePolicies'{_larpPathPrefix :: Maybe Text, _larpMaxItems :: Maybe Nat, _larpMarker :: Maybe Text, _larpRoleName :: Text} deriving (Eq, Read, Show)

-- | 'ListAttachedRolePolicies' smart constructor.
listAttachedRolePolicies :: Text -> ListAttachedRolePolicies
listAttachedRolePolicies pRoleName = ListAttachedRolePolicies'{_larpPathPrefix = Nothing, _larpMaxItems = Nothing, _larpMarker = Nothing, _larpRoleName = pRoleName};

-- | The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- policies.
larpPathPrefix :: Lens' ListAttachedRolePolicies (Maybe Text)
larpPathPrefix = lens _larpPathPrefix (\ s a -> s{_larpPathPrefix = a});

-- | Use this only when paginating results to indicate the maximum number of
-- policies you want in the response. If there are additional policies
-- beyond the maximum you specify, the @IsTruncated@ response element is
-- @true@. This parameter is optional. If you do not include it, it
-- defaults to 100.
larpMaxItems :: Lens' ListAttachedRolePolicies (Maybe Natural)
larpMaxItems = lens _larpMaxItems (\ s a -> s{_larpMaxItems = a}) . mapping _Nat;

-- | Use this only when paginating results, and only in a subsequent request
-- after you\'ve received a response where the results are truncated. Set
-- it to the value of the @Marker@ element in the response you just
-- received.
larpMarker :: Lens' ListAttachedRolePolicies (Maybe Text)
larpMarker = lens _larpMarker (\ s a -> s{_larpMarker = a});

-- | The name (friendly name, not ARN) of the role to list attached policies
-- for.
larpRoleName :: Lens' ListAttachedRolePolicies Text
larpRoleName = lens _larpRoleName (\ s a -> s{_larpRoleName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest ListAttachedRolePolicies where
        type Sv ListAttachedRolePolicies = IAM
        type Rs ListAttachedRolePolicies =
             ListAttachedRolePoliciesResponse
        request = post
        response
          = receiveXMLWrapper "ListAttachedRolePoliciesResult"
              (\ s h x ->
                 ListAttachedRolePoliciesResponse' <$>
                   (x .@? "AttachedPolicies" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "Marker")
                     <*> (x .@? "IsTruncated"))

instance ToHeaders ListAttachedRolePolicies where
        toHeaders = const mempty

instance ToPath ListAttachedRolePolicies where
        toPath = const "/"

instance ToQuery ListAttachedRolePolicies where
        toQuery ListAttachedRolePolicies'{..}
          = mconcat
              ["Action" =:
                 ("ListAttachedRolePolicies" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _larpPathPrefix,
               "MaxItems" =: _larpMaxItems, "Marker" =: _larpMarker,
               "RoleName" =: _larpRoleName]

-- | /See:/ 'listAttachedRolePoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'larprAttachedPolicies'
--
-- * 'larprMarker'
--
-- * 'larprIsTruncated'
data ListAttachedRolePoliciesResponse = ListAttachedRolePoliciesResponse'{_larprAttachedPolicies :: Maybe [AttachedPolicy], _larprMarker :: Maybe Text, _larprIsTruncated :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'ListAttachedRolePoliciesResponse' smart constructor.
listAttachedRolePoliciesResponse :: ListAttachedRolePoliciesResponse
listAttachedRolePoliciesResponse = ListAttachedRolePoliciesResponse'{_larprAttachedPolicies = Nothing, _larprMarker = Nothing, _larprIsTruncated = Nothing};

-- | A list of the attached policies.
larprAttachedPolicies :: Lens' ListAttachedRolePoliciesResponse [AttachedPolicy]
larprAttachedPolicies = lens _larprAttachedPolicies (\ s a -> s{_larprAttachedPolicies = a}) . _Default;

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
larprMarker :: Lens' ListAttachedRolePoliciesResponse (Maybe Text)
larprMarker = lens _larprMarker (\ s a -> s{_larprMarker = a});

-- | A flag that indicates whether there are more policies to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more policies in the
-- list.
larprIsTruncated :: Lens' ListAttachedRolePoliciesResponse (Maybe Bool)
larprIsTruncated = lens _larprIsTruncated (\ s a -> s{_larprIsTruncated = a});
