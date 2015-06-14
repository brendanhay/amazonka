{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.GetGroup
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

-- | Returns a list of users that are in the specified group. You can
-- paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetGroup.html>
module Network.AWS.IAM.GetGroup
    (
    -- * Request
      GetGroup
    -- ** Request constructor
    , getGroup
    -- ** Request lenses
    , ggGroupName
    , ggMaxItems
    , ggMarker

    -- * Response
    , GetGroupResponse
    -- ** Response constructor
    , getGroupResponse
    -- ** Response lenses
    , ggrIsTruncated
    , ggrGroup
    , ggrUsers
    , ggrMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'getGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ggGroupName'
--
-- * 'ggMaxItems'
--
-- * 'ggMarker'
data GetGroup = GetGroup'{_ggGroupName :: Text, _ggMaxItems :: Nat, _ggMarker :: Text} deriving (Eq, Read, Show)

-- | 'GetGroup' smart constructor.
getGroup :: Text -> Natural -> Text -> GetGroup
getGroup pGroupName pMaxItems pMarker = GetGroup'{_ggGroupName = pGroupName, _ggMaxItems = _Nat # pMaxItems, _ggMarker = pMarker};

-- | The name of the group.
ggGroupName :: Lens' GetGroup Text
ggGroupName = lens _ggGroupName (\ s a -> s{_ggGroupName = a});

-- | Use this only when paginating results to indicate the maximum number of
-- groups you want in the response. If there are additional groups beyond
-- the maximum you specify, the @IsTruncated@ response element is @true@.
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
ggMaxItems :: Lens' GetGroup Natural
ggMaxItems = lens _ggMaxItems (\ s a -> s{_ggMaxItems = a}) . _Nat;

-- | Use this only when paginating results, and only in a subsequent request
-- after you\'ve received a response where the results are truncated. Set
-- it to the value of the @Marker@ element in the response you just
-- received.
ggMarker :: Lens' GetGroup Text
ggMarker = lens _ggMarker (\ s a -> s{_ggMarker = a});

instance AWSRequest GetGroup where
        type Sv GetGroup = IAM
        type Rs GetGroup = GetGroupResponse
        request = post
        response
          = receiveXMLWrapper "GetGroupResult"
              (\ s h x ->
                 GetGroupResponse' <$>
                   x .@? "IsTruncated" <*> x .@ "Group" <*>
                     (x .@? "Users" .!@ mempty >>= parseXMLList "member")
                     <*> x .@ "Marker")

instance ToHeaders GetGroup where
        toHeaders = const mempty

instance ToPath GetGroup where
        toPath = const "/"

instance ToQuery GetGroup where
        toQuery GetGroup'{..}
          = mconcat
              ["Action" =: ("GetGroup" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "GroupName" =: _ggGroupName,
               "MaxItems" =: _ggMaxItems, "Marker" =: _ggMarker]

-- | /See:/ 'getGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ggrIsTruncated'
--
-- * 'ggrGroup'
--
-- * 'ggrUsers'
--
-- * 'ggrMarker'
data GetGroupResponse = GetGroupResponse'{_ggrIsTruncated :: Maybe Bool, _ggrGroup :: Group, _ggrUsers :: [User], _ggrMarker :: Text} deriving (Eq, Read, Show)

-- | 'GetGroupResponse' smart constructor.
getGroupResponse :: Group -> [User] -> Text -> GetGroupResponse
getGroupResponse pGroup pUsers pMarker = GetGroupResponse'{_ggrIsTruncated = Nothing, _ggrGroup = pGroup, _ggrUsers = pUsers, _ggrMarker = pMarker};

-- | A flag that indicates whether there are more user names to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more user names in the
-- list.
ggrIsTruncated :: Lens' GetGroupResponse (Maybe Bool)
ggrIsTruncated = lens _ggrIsTruncated (\ s a -> s{_ggrIsTruncated = a});

-- | Information about the group.
ggrGroup :: Lens' GetGroupResponse Group
ggrGroup = lens _ggrGroup (\ s a -> s{_ggrGroup = a});

-- | A list of users in the group.
ggrUsers :: Lens' GetGroupResponse [User]
ggrUsers = lens _ggrUsers (\ s a -> s{_ggrUsers = a});

-- | If IsTruncated is @true@, then this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
ggrMarker :: Lens' GetGroupResponse Text
ggrMarker = lens _ggrMarker (\ s a -> s{_ggrMarker = a});
