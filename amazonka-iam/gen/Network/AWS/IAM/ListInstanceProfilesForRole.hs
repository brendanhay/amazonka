{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.ListInstanceProfilesForRole
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

-- | Lists the instance profiles that have the specified associated role. If
-- there are none, the action returns an empty list. For more information
-- about instance profiles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles>.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListInstanceProfilesForRole.html>
module Network.AWS.IAM.ListInstanceProfilesForRole
    (
    -- * Request
      ListInstanceProfilesForRole
    -- ** Request constructor
    , listInstanceProfilesForRole
    -- ** Request lenses
    , lipfrRoleName
    , lipfrMaxItems
    , lipfrMarker

    -- * Response
    , ListInstanceProfilesForRoleResponse
    -- ** Response constructor
    , listInstanceProfilesForRoleResponse
    -- ** Response lenses
    , lipfrrIsTruncated
    , lipfrrInstanceProfiles
    , lipfrrMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'listInstanceProfilesForRole' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lipfrRoleName'
--
-- * 'lipfrMaxItems'
--
-- * 'lipfrMarker'
data ListInstanceProfilesForRole = ListInstanceProfilesForRole'{_lipfrRoleName :: Text, _lipfrMaxItems :: Nat, _lipfrMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListInstanceProfilesForRole' smart constructor.
listInstanceProfilesForRole :: Text -> Natural -> Text -> ListInstanceProfilesForRole
listInstanceProfilesForRole pRoleName pMaxItems pMarker = ListInstanceProfilesForRole'{_lipfrRoleName = pRoleName, _lipfrMaxItems = _Nat # pMaxItems, _lipfrMarker = pMarker};

-- | The name of the role to list instance profiles for.
lipfrRoleName :: Lens' ListInstanceProfilesForRole Text
lipfrRoleName = lens _lipfrRoleName (\ s a -> s{_lipfrRoleName = a});

-- | Use this parameter only when paginating results to indicate the maximum
-- number of instance profiles you want in the response. If there are
-- additional instance profiles beyond the maximum you specify, the
-- @IsTruncated@ response element is @true@. This parameter is optional. If
-- you do not include it, it defaults to 100.
lipfrMaxItems :: Lens' ListInstanceProfilesForRole Natural
lipfrMaxItems = lens _lipfrMaxItems (\ s a -> s{_lipfrMaxItems = a}) . _Nat;

-- | Use this parameter only when paginating results, and only in a
-- subsequent request after you\'ve received a response where the results
-- are truncated. Set it to the value of the @Marker@ element in the
-- response you just received.
lipfrMarker :: Lens' ListInstanceProfilesForRole Text
lipfrMarker = lens _lipfrMarker (\ s a -> s{_lipfrMarker = a});

instance AWSRequest ListInstanceProfilesForRole where
        type Sv ListInstanceProfilesForRole = IAM
        type Rs ListInstanceProfilesForRole =
             ListInstanceProfilesForRoleResponse
        request = post
        response
          = receiveXMLWrapper
              "ListInstanceProfilesForRoleResult"
              (\ s h x ->
                 ListInstanceProfilesForRoleResponse' <$>
                   x .@? "IsTruncated" <*>
                     (x .@? "InstanceProfiles" .!@ mempty >>=
                        parseXMLList "member")
                     <*> x .@ "Marker")

instance ToHeaders ListInstanceProfilesForRole where
        toHeaders = const mempty

instance ToPath ListInstanceProfilesForRole where
        toPath = const "/"

instance ToQuery ListInstanceProfilesForRole where
        toQuery ListInstanceProfilesForRole'{..}
          = mconcat
              ["Action" =:
                 ("ListInstanceProfilesForRole" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RoleName" =: _lipfrRoleName,
               "MaxItems" =: _lipfrMaxItems,
               "Marker" =: _lipfrMarker]

-- | /See:/ 'listInstanceProfilesForRoleResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lipfrrIsTruncated'
--
-- * 'lipfrrInstanceProfiles'
--
-- * 'lipfrrMarker'
data ListInstanceProfilesForRoleResponse = ListInstanceProfilesForRoleResponse'{_lipfrrIsTruncated :: Maybe Bool, _lipfrrInstanceProfiles :: [InstanceProfile], _lipfrrMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListInstanceProfilesForRoleResponse' smart constructor.
listInstanceProfilesForRoleResponse :: [InstanceProfile] -> Text -> ListInstanceProfilesForRoleResponse
listInstanceProfilesForRoleResponse pInstanceProfiles pMarker = ListInstanceProfilesForRoleResponse'{_lipfrrIsTruncated = Nothing, _lipfrrInstanceProfiles = pInstanceProfiles, _lipfrrMarker = pMarker};

-- | A flag that indicates whether there are more instance profiles to list.
-- If your results were truncated, you can make a subsequent pagination
-- request using the @Marker@ request parameter to retrieve more instance
-- profiles in the list.
lipfrrIsTruncated :: Lens' ListInstanceProfilesForRoleResponse (Maybe Bool)
lipfrrIsTruncated = lens _lipfrrIsTruncated (\ s a -> s{_lipfrrIsTruncated = a});

-- | A list of instance profiles.
lipfrrInstanceProfiles :: Lens' ListInstanceProfilesForRoleResponse [InstanceProfile]
lipfrrInstanceProfiles = lens _lipfrrInstanceProfiles (\ s a -> s{_lipfrrInstanceProfiles = a});

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lipfrrMarker :: Lens' ListInstanceProfilesForRoleResponse Text
lipfrrMarker = lens _lipfrrMarker (\ s a -> s{_lipfrrMarker = a});
