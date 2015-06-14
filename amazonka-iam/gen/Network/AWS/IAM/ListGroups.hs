{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.ListGroups
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

-- | Lists the groups that have the specified path prefix.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroups.html>
module Network.AWS.IAM.ListGroups
    (
    -- * Request
      ListGroups
    -- ** Request constructor
    , listGroups
    -- ** Request lenses
    , lgPathPrefix
    , lgMaxItems
    , lgMarker

    -- * Response
    , ListGroupsResponse
    -- ** Response constructor
    , listGroupsResponse
    -- ** Response lenses
    , lgrIsTruncated
    , lgrGroups
    , lgrMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'listGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgPathPrefix'
--
-- * 'lgMaxItems'
--
-- * 'lgMarker'
data ListGroups = ListGroups'{_lgPathPrefix :: Text, _lgMaxItems :: Nat, _lgMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListGroups' smart constructor.
listGroups :: Text -> Natural -> Text -> ListGroups
listGroups pPathPrefix pMaxItems pMarker = ListGroups'{_lgPathPrefix = pPathPrefix, _lgMaxItems = _Nat # pMaxItems, _lgMarker = pMarker};

-- | The path prefix for filtering the results. For example, the prefix
-- @\/division_abc\/subdivision_xyz\/@ gets all groups whose path starts
-- with @\/division_abc\/subdivision_xyz\/@.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/), listing all groups.
lgPathPrefix :: Lens' ListGroups Text
lgPathPrefix = lens _lgPathPrefix (\ s a -> s{_lgPathPrefix = a});

-- | Use this only when paginating results to indicate the maximum number of
-- groups you want in the response. If there are additional groups beyond
-- the maximum you specify, the @IsTruncated@ response element is @true@.
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lgMaxItems :: Lens' ListGroups Natural
lgMaxItems = lens _lgMaxItems (\ s a -> s{_lgMaxItems = a}) . _Nat;

-- | Use this only when paginating results, and only in a subsequent request
-- after you\'ve received a response where the results are truncated. Set
-- it to the value of the @Marker@ element in the response you just
-- received.
lgMarker :: Lens' ListGroups Text
lgMarker = lens _lgMarker (\ s a -> s{_lgMarker = a});

instance AWSRequest ListGroups where
        type Sv ListGroups = IAM
        type Rs ListGroups = ListGroupsResponse
        request = post
        response
          = receiveXMLWrapper "ListGroupsResult"
              (\ s h x ->
                 ListGroupsResponse' <$>
                   x .@? "IsTruncated" <*>
                     (x .@? "Groups" .!@ mempty >>= parseXMLList "member")
                     <*> x .@ "Marker")

instance ToHeaders ListGroups where
        toHeaders = const mempty

instance ToPath ListGroups where
        toPath = const "/"

instance ToQuery ListGroups where
        toQuery ListGroups'{..}
          = mconcat
              ["Action" =: ("ListGroups" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _lgPathPrefix,
               "MaxItems" =: _lgMaxItems, "Marker" =: _lgMarker]

-- | /See:/ 'listGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgrIsTruncated'
--
-- * 'lgrGroups'
--
-- * 'lgrMarker'
data ListGroupsResponse = ListGroupsResponse'{_lgrIsTruncated :: Maybe Bool, _lgrGroups :: [Group], _lgrMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListGroupsResponse' smart constructor.
listGroupsResponse :: [Group] -> Text -> ListGroupsResponse
listGroupsResponse pGroups pMarker = ListGroupsResponse'{_lgrIsTruncated = Nothing, _lgrGroups = pGroups, _lgrMarker = pMarker};

-- | A flag that indicates whether there are more groups to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more groups in the
-- list.
lgrIsTruncated :: Lens' ListGroupsResponse (Maybe Bool)
lgrIsTruncated = lens _lgrIsTruncated (\ s a -> s{_lgrIsTruncated = a});

-- | A list of groups.
lgrGroups :: Lens' ListGroupsResponse [Group]
lgrGroups = lens _lgrGroups (\ s a -> s{_lgrGroups = a});

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lgrMarker :: Lens' ListGroupsResponse Text
lgrMarker = lens _lgrMarker (\ s a -> s{_lgrMarker = a});
