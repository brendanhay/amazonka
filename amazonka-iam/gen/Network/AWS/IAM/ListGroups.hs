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
    , lgrMarker
    , lgrIsTruncated
    , lgrGroups
    , lgrStatusCode
    ) where

import Network.AWS.IAM.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgPathPrefix'
--
-- * 'lgMaxItems'
--
-- * 'lgMarker'
data ListGroups = ListGroups'{_lgPathPrefix :: Maybe Text, _lgMaxItems :: Maybe Nat, _lgMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListGroups' smart constructor.
listGroups :: ListGroups
listGroups = ListGroups'{_lgPathPrefix = Nothing, _lgMaxItems = Nothing, _lgMarker = Nothing};

-- | The path prefix for filtering the results. For example, the prefix
-- @\/division_abc\/subdivision_xyz\/@ gets all groups whose path starts
-- with @\/division_abc\/subdivision_xyz\/@.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/), listing all groups.
lgPathPrefix :: Lens' ListGroups (Maybe Text)
lgPathPrefix = lens _lgPathPrefix (\ s a -> s{_lgPathPrefix = a});

-- | Use this only when paginating results to indicate the maximum number of
-- groups you want in the response. If there are additional groups beyond
-- the maximum you specify, the @IsTruncated@ response element is @true@.
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lgMaxItems :: Lens' ListGroups (Maybe Natural)
lgMaxItems = lens _lgMaxItems (\ s a -> s{_lgMaxItems = a}) . mapping _Nat;

-- | Use this only when paginating results, and only in a subsequent request
-- after you\'ve received a response where the results are truncated. Set
-- it to the value of the @Marker@ element in the response you just
-- received.
lgMarker :: Lens' ListGroups (Maybe Text)
lgMarker = lens _lgMarker (\ s a -> s{_lgMarker = a});

instance AWSPager ListGroups where
        page rq rs
          | stop (rs ^. lgrIsTruncated) = Nothing
          | isNothing (rs ^. lgrMarker) = Nothing
          | otherwise = Just $ rq & lgMarker .~ rs ^. lgrMarker

instance AWSRequest ListGroups where
        type Sv ListGroups = IAM
        type Rs ListGroups = ListGroupsResponse
        request = post
        response
          = receiveXMLWrapper "ListGroupsResult"
              (\ s h x ->
                 ListGroupsResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (x .@? "Groups" .!@ mempty >>= parseXMLList "member")
                     <*> (pure (fromEnum s)))

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

-- | Contains the response to a successful ListGroups request.
--
-- /See:/ 'listGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgrMarker'
--
-- * 'lgrIsTruncated'
--
-- * 'lgrGroups'
--
-- * 'lgrStatusCode'
data ListGroupsResponse = ListGroupsResponse'{_lgrMarker :: Maybe Text, _lgrIsTruncated :: Maybe Bool, _lgrGroups :: [Group], _lgrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'ListGroupsResponse' smart constructor.
listGroupsResponse :: Int -> ListGroupsResponse
listGroupsResponse pStatusCode = ListGroupsResponse'{_lgrMarker = Nothing, _lgrIsTruncated = Nothing, _lgrGroups = mempty, _lgrStatusCode = pStatusCode};

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lgrMarker :: Lens' ListGroupsResponse (Maybe Text)
lgrMarker = lens _lgrMarker (\ s a -> s{_lgrMarker = a});

-- | A flag that indicates whether there are more groups to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more groups in the
-- list.
lgrIsTruncated :: Lens' ListGroupsResponse (Maybe Bool)
lgrIsTruncated = lens _lgrIsTruncated (\ s a -> s{_lgrIsTruncated = a});

-- | A list of groups.
lgrGroups :: Lens' ListGroupsResponse [Group]
lgrGroups = lens _lgrGroups (\ s a -> s{_lgrGroups = a});

-- | FIXME: Undocumented member.
lgrStatusCode :: Lens' ListGroupsResponse Int
lgrStatusCode = lens _lgrStatusCode (\ s a -> s{_lgrStatusCode = a});
