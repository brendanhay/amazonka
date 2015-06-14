{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.ListGroupsForUser
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

-- | Lists the groups the specified user belongs to.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroupsForUser.html>
module Network.AWS.IAM.ListGroupsForUser
    (
    -- * Request
      ListGroupsForUser
    -- ** Request constructor
    , listGroupsForUser
    -- ** Request lenses
    , lgfuUserName
    , lgfuMaxItems
    , lgfuMarker

    -- * Response
    , ListGroupsForUserResponse
    -- ** Response constructor
    , listGroupsForUserResponse
    -- ** Response lenses
    , lgfurIsTruncated
    , lgfurGroups
    , lgfurMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'listGroupsForUser' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgfuUserName'
--
-- * 'lgfuMaxItems'
--
-- * 'lgfuMarker'
data ListGroupsForUser = ListGroupsForUser'{_lgfuUserName :: Text, _lgfuMaxItems :: Nat, _lgfuMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListGroupsForUser' smart constructor.
listGroupsForUser :: Text -> Natural -> Text -> ListGroupsForUser
listGroupsForUser pUserName pMaxItems pMarker = ListGroupsForUser'{_lgfuUserName = pUserName, _lgfuMaxItems = _Nat # pMaxItems, _lgfuMarker = pMarker};

-- | The name of the user to list groups for.
lgfuUserName :: Lens' ListGroupsForUser Text
lgfuUserName = lens _lgfuUserName (\ s a -> s{_lgfuUserName = a});

-- | Use this only when paginating results to indicate the maximum number of
-- groups you want in the response. If there are additional groups beyond
-- the maximum you specify, the @IsTruncated@ response element is @true@.
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lgfuMaxItems :: Lens' ListGroupsForUser Natural
lgfuMaxItems = lens _lgfuMaxItems (\ s a -> s{_lgfuMaxItems = a}) . _Nat;

-- | Use this only when paginating results, and only in a subsequent request
-- after you\'ve received a response where the results are truncated. Set
-- it to the value of the @Marker@ element in the response you just
-- received.
lgfuMarker :: Lens' ListGroupsForUser Text
lgfuMarker = lens _lgfuMarker (\ s a -> s{_lgfuMarker = a});

instance AWSRequest ListGroupsForUser where
        type Sv ListGroupsForUser = IAM
        type Rs ListGroupsForUser = ListGroupsForUserResponse
        request = post
        response
          = receiveXMLWrapper "ListGroupsForUserResult"
              (\ s h x ->
                 ListGroupsForUserResponse' <$>
                   x .@? "IsTruncated" <*>
                     (x .@? "Groups" .!@ mempty >>= parseXMLList "member")
                     <*> x .@ "Marker")

instance ToHeaders ListGroupsForUser where
        toHeaders = const mempty

instance ToPath ListGroupsForUser where
        toPath = const "/"

instance ToQuery ListGroupsForUser where
        toQuery ListGroupsForUser'{..}
          = mconcat
              ["Action" =: ("ListGroupsForUser" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _lgfuUserName,
               "MaxItems" =: _lgfuMaxItems, "Marker" =: _lgfuMarker]

-- | /See:/ 'listGroupsForUserResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgfurIsTruncated'
--
-- * 'lgfurGroups'
--
-- * 'lgfurMarker'
data ListGroupsForUserResponse = ListGroupsForUserResponse'{_lgfurIsTruncated :: Maybe Bool, _lgfurGroups :: [Group], _lgfurMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListGroupsForUserResponse' smart constructor.
listGroupsForUserResponse :: [Group] -> Text -> ListGroupsForUserResponse
listGroupsForUserResponse pGroups pMarker = ListGroupsForUserResponse'{_lgfurIsTruncated = Nothing, _lgfurGroups = pGroups, _lgfurMarker = pMarker};

-- | A flag that indicates whether there are more groups to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more groups in the
-- list.
lgfurIsTruncated :: Lens' ListGroupsForUserResponse (Maybe Bool)
lgfurIsTruncated = lens _lgfurIsTruncated (\ s a -> s{_lgfurIsTruncated = a});

-- | A list of groups.
lgfurGroups :: Lens' ListGroupsForUserResponse [Group]
lgfurGroups = lens _lgfurGroups (\ s a -> s{_lgfurGroups = a});

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lgfurMarker :: Lens' ListGroupsForUserResponse Text
lgfurMarker = lens _lgfurMarker (\ s a -> s{_lgfurMarker = a});
