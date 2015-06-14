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
    , lgfuMaxItems
    , lgfuMarker
    , lgfuUserName

    -- * Response
    , ListGroupsForUserResponse
    -- ** Response constructor
    , listGroupsForUserResponse
    -- ** Response lenses
    , lgfurMarker
    , lgfurIsTruncated
    , lgfurGroups
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'listGroupsForUser' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgfuMaxItems'
--
-- * 'lgfuMarker'
--
-- * 'lgfuUserName'
data ListGroupsForUser = ListGroupsForUser'{_lgfuMaxItems :: Maybe Nat, _lgfuMarker :: Maybe Text, _lgfuUserName :: Text} deriving (Eq, Read, Show)

-- | 'ListGroupsForUser' smart constructor.
listGroupsForUser :: Text -> ListGroupsForUser
listGroupsForUser pUserName = ListGroupsForUser'{_lgfuMaxItems = Nothing, _lgfuMarker = Nothing, _lgfuUserName = pUserName};

-- | Use this only when paginating results to indicate the maximum number of
-- groups you want in the response. If there are additional groups beyond
-- the maximum you specify, the @IsTruncated@ response element is @true@.
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lgfuMaxItems :: Lens' ListGroupsForUser (Maybe Natural)
lgfuMaxItems = lens _lgfuMaxItems (\ s a -> s{_lgfuMaxItems = a}) . mapping _Nat;

-- | Use this only when paginating results, and only in a subsequent request
-- after you\'ve received a response where the results are truncated. Set
-- it to the value of the @Marker@ element in the response you just
-- received.
lgfuMarker :: Lens' ListGroupsForUser (Maybe Text)
lgfuMarker = lens _lgfuMarker (\ s a -> s{_lgfuMarker = a});

-- | The name of the user to list groups for.
lgfuUserName :: Lens' ListGroupsForUser Text
lgfuUserName = lens _lgfuUserName (\ s a -> s{_lgfuUserName = a});

instance AWSRequest ListGroupsForUser where
        type Sv ListGroupsForUser = IAM
        type Rs ListGroupsForUser = ListGroupsForUserResponse
        request = post
        response
          = receiveXMLWrapper "ListGroupsForUserResult"
              (\ s h x ->
                 ListGroupsForUserResponse' <$>
                   x .@? "Marker" <*> x .@? "IsTruncated" <*>
                     (x .@? "Groups" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListGroupsForUser where
        toHeaders = const mempty

instance ToPath ListGroupsForUser where
        toPath = const "/"

instance ToQuery ListGroupsForUser where
        toQuery ListGroupsForUser'{..}
          = mconcat
              ["Action" =: ("ListGroupsForUser" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "MaxItems" =: _lgfuMaxItems, "Marker" =: _lgfuMarker,
               "UserName" =: _lgfuUserName]

-- | /See:/ 'listGroupsForUserResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgfurMarker'
--
-- * 'lgfurIsTruncated'
--
-- * 'lgfurGroups'
data ListGroupsForUserResponse = ListGroupsForUserResponse'{_lgfurMarker :: Maybe Text, _lgfurIsTruncated :: Maybe Bool, _lgfurGroups :: [Group]} deriving (Eq, Read, Show)

-- | 'ListGroupsForUserResponse' smart constructor.
listGroupsForUserResponse :: ListGroupsForUserResponse
listGroupsForUserResponse = ListGroupsForUserResponse'{_lgfurMarker = Nothing, _lgfurIsTruncated = Nothing, _lgfurGroups = mempty};

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lgfurMarker :: Lens' ListGroupsForUserResponse (Maybe Text)
lgfurMarker = lens _lgfurMarker (\ s a -> s{_lgfurMarker = a});

-- | A flag that indicates whether there are more groups to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more groups in the
-- list.
lgfurIsTruncated :: Lens' ListGroupsForUserResponse (Maybe Bool)
lgfurIsTruncated = lens _lgfurIsTruncated (\ s a -> s{_lgfurIsTruncated = a});

-- | A list of groups.
lgfurGroups :: Lens' ListGroupsForUserResponse [Group]
lgfurGroups = lens _lgfurGroups (\ s a -> s{_lgfurGroups = a});
