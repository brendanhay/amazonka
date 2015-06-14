{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.ListAttachedUserPolicies
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

-- | Lists all managed policies that are attached to the specified user.
--
-- A user can also have inline policies embedded with it. To list the
-- inline policies for a user, use the ListUserPolicies API. For
-- information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters. You can use the @PathPrefix@ parameter to limit the list of
-- policies to only those matching the specified path prefix. If there are
-- no policies attached to the specified group (or none that match the
-- specified path prefix), the action returns an empty list.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAttachedUserPolicies.html>
module Network.AWS.IAM.ListAttachedUserPolicies
    (
    -- * Request
      ListAttachedUserPolicies
    -- ** Request constructor
    , listAttachedUserPolicies
    -- ** Request lenses
    , laupPathPrefix
    , laupUserName
    , laupMaxItems
    , laupMarker

    -- * Response
    , ListAttachedUserPoliciesResponse
    -- ** Response constructor
    , listAttachedUserPoliciesResponse
    -- ** Response lenses
    , lauprAttachedPolicies
    , lauprIsTruncated
    , lauprMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'listAttachedUserPolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'laupPathPrefix'
--
-- * 'laupUserName'
--
-- * 'laupMaxItems'
--
-- * 'laupMarker'
data ListAttachedUserPolicies = ListAttachedUserPolicies'{_laupPathPrefix :: Maybe Text, _laupUserName :: Text, _laupMaxItems :: Nat, _laupMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListAttachedUserPolicies' smart constructor.
listAttachedUserPolicies :: Text -> Natural -> Text -> ListAttachedUserPolicies
listAttachedUserPolicies pUserName pMaxItems pMarker = ListAttachedUserPolicies'{_laupPathPrefix = Nothing, _laupUserName = pUserName, _laupMaxItems = _Nat # pMaxItems, _laupMarker = pMarker};

-- | The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- policies.
laupPathPrefix :: Lens' ListAttachedUserPolicies (Maybe Text)
laupPathPrefix = lens _laupPathPrefix (\ s a -> s{_laupPathPrefix = a});

-- | The name (friendly name, not ARN) of the user to list attached policies
-- for.
laupUserName :: Lens' ListAttachedUserPolicies Text
laupUserName = lens _laupUserName (\ s a -> s{_laupUserName = a});

-- | Use this only when paginating results to indicate the maximum number of
-- policies you want in the response. If there are additional policies
-- beyond the maximum you specify, the @IsTruncated@ response element is
-- @true@. This parameter is optional. If you do not include it, it
-- defaults to 100.
laupMaxItems :: Lens' ListAttachedUserPolicies Natural
laupMaxItems = lens _laupMaxItems (\ s a -> s{_laupMaxItems = a}) . _Nat;

-- | Use this only when paginating results, and only in a subsequent request
-- after you\'ve received a response where the results are truncated. Set
-- it to the value of the @Marker@ element in the response you just
-- received.
laupMarker :: Lens' ListAttachedUserPolicies Text
laupMarker = lens _laupMarker (\ s a -> s{_laupMarker = a});

instance AWSRequest ListAttachedUserPolicies where
        type Sv ListAttachedUserPolicies = IAM
        type Rs ListAttachedUserPolicies =
             ListAttachedUserPoliciesResponse
        request = post
        response
          = receiveXMLWrapper "ListAttachedUserPoliciesResult"
              (\ s h x ->
                 ListAttachedUserPoliciesResponse' <$>
                   (x .@? "AttachedPolicies" .!@ mempty >>=
                      parseXMLList "member")
                     <*> x .@? "IsTruncated"
                     <*> x .@ "Marker")

instance ToHeaders ListAttachedUserPolicies where
        toHeaders = const mempty

instance ToPath ListAttachedUserPolicies where
        toPath = const "/"

instance ToQuery ListAttachedUserPolicies where
        toQuery ListAttachedUserPolicies'{..}
          = mconcat
              ["Action" =:
                 ("ListAttachedUserPolicies" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _laupPathPrefix,
               "UserName" =: _laupUserName,
               "MaxItems" =: _laupMaxItems, "Marker" =: _laupMarker]

-- | /See:/ 'listAttachedUserPoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lauprAttachedPolicies'
--
-- * 'lauprIsTruncated'
--
-- * 'lauprMarker'
data ListAttachedUserPoliciesResponse = ListAttachedUserPoliciesResponse'{_lauprAttachedPolicies :: [AttachedPolicy], _lauprIsTruncated :: Maybe Bool, _lauprMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListAttachedUserPoliciesResponse' smart constructor.
listAttachedUserPoliciesResponse :: Text -> ListAttachedUserPoliciesResponse
listAttachedUserPoliciesResponse pMarker = ListAttachedUserPoliciesResponse'{_lauprAttachedPolicies = mempty, _lauprIsTruncated = Nothing, _lauprMarker = pMarker};

-- | A list of the attached policies.
lauprAttachedPolicies :: Lens' ListAttachedUserPoliciesResponse [AttachedPolicy]
lauprAttachedPolicies = lens _lauprAttachedPolicies (\ s a -> s{_lauprAttachedPolicies = a});

-- | A flag that indicates whether there are more policies to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more policies in the
-- list.
lauprIsTruncated :: Lens' ListAttachedUserPoliciesResponse (Maybe Bool)
lauprIsTruncated = lens _lauprIsTruncated (\ s a -> s{_lauprIsTruncated = a});

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lauprMarker :: Lens' ListAttachedUserPoliciesResponse Text
lauprMarker = lens _lauprMarker (\ s a -> s{_lauprMarker = a});
