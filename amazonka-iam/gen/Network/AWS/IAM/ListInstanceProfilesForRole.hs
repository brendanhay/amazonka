{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.IAM.ListInstanceProfilesForRole
-- Copyright   : (c) 2013-2015 Brendan Hay
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
    , lipfrMaxItems
    , lipfrMarker
    , lipfrRoleName

    -- * Response
    , ListInstanceProfilesForRoleResponse
    -- ** Response constructor
    , listInstanceProfilesForRoleResponse
    -- ** Response lenses
    , lipfrrMarker
    , lipfrrIsTruncated
    , lipfrrStatus
    , lipfrrInstanceProfiles
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listInstanceProfilesForRole' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lipfrMaxItems'
--
-- * 'lipfrMarker'
--
-- * 'lipfrRoleName'
data ListInstanceProfilesForRole = ListInstanceProfilesForRole'
    { _lipfrMaxItems :: !(Maybe Nat)
    , _lipfrMarker   :: !(Maybe Text)
    , _lipfrRoleName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListInstanceProfilesForRole' smart constructor.
listInstanceProfilesForRole :: Text -> ListInstanceProfilesForRole
listInstanceProfilesForRole pRoleName =
    ListInstanceProfilesForRole'
    { _lipfrMaxItems = Nothing
    , _lipfrMarker = Nothing
    , _lipfrRoleName = pRoleName
    }

-- | Use this parameter only when paginating results to indicate the maximum
-- number of instance profiles you want in the response. If there are
-- additional instance profiles beyond the maximum you specify, the
-- @IsTruncated@ response element is @true@. This parameter is optional. If
-- you do not include it, it defaults to 100.
lipfrMaxItems :: Lens' ListInstanceProfilesForRole (Maybe Natural)
lipfrMaxItems = lens _lipfrMaxItems (\ s a -> s{_lipfrMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results, and only in a
-- subsequent request after you\'ve received a response where the results
-- are truncated. Set it to the value of the @Marker@ element in the
-- response you just received.
lipfrMarker :: Lens' ListInstanceProfilesForRole (Maybe Text)
lipfrMarker = lens _lipfrMarker (\ s a -> s{_lipfrMarker = a});

-- | The name of the role to list instance profiles for.
lipfrRoleName :: Lens' ListInstanceProfilesForRole Text
lipfrRoleName = lens _lipfrRoleName (\ s a -> s{_lipfrRoleName = a});

instance AWSPager ListInstanceProfilesForRole where
        page rq rs
          | stop (rs ^. lipfrrIsTruncated) = Nothing
          | isNothing (rs ^. lipfrrMarker) = Nothing
          | otherwise =
            Just $ rq & lipfrMarker .~ rs ^. lipfrrMarker

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
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "InstanceProfiles" .!@ mempty >>=
                        parseXMLList "member"))

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
               "MaxItems" =: _lipfrMaxItems,
               "Marker" =: _lipfrMarker,
               "RoleName" =: _lipfrRoleName]

-- | Contains the response to a successful ListInstanceProfilesForRole
-- request.
--
-- /See:/ 'listInstanceProfilesForRoleResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lipfrrMarker'
--
-- * 'lipfrrIsTruncated'
--
-- * 'lipfrrStatus'
--
-- * 'lipfrrInstanceProfiles'
data ListInstanceProfilesForRoleResponse = ListInstanceProfilesForRoleResponse'
    { _lipfrrMarker           :: !(Maybe Text)
    , _lipfrrIsTruncated      :: !(Maybe Bool)
    , _lipfrrStatus           :: !Int
    , _lipfrrInstanceProfiles :: ![InstanceProfile]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListInstanceProfilesForRoleResponse' smart constructor.
listInstanceProfilesForRoleResponse :: Int -> ListInstanceProfilesForRoleResponse
listInstanceProfilesForRoleResponse pStatus =
    ListInstanceProfilesForRoleResponse'
    { _lipfrrMarker = Nothing
    , _lipfrrIsTruncated = Nothing
    , _lipfrrStatus = pStatus
    , _lipfrrInstanceProfiles = mempty
    }

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lipfrrMarker :: Lens' ListInstanceProfilesForRoleResponse (Maybe Text)
lipfrrMarker = lens _lipfrrMarker (\ s a -> s{_lipfrrMarker = a});

-- | A flag that indicates whether there are more instance profiles to list.
-- If your results were truncated, you can make a subsequent pagination
-- request using the @Marker@ request parameter to retrieve more instance
-- profiles in the list.
lipfrrIsTruncated :: Lens' ListInstanceProfilesForRoleResponse (Maybe Bool)
lipfrrIsTruncated = lens _lipfrrIsTruncated (\ s a -> s{_lipfrrIsTruncated = a});

-- | FIXME: Undocumented member.
lipfrrStatus :: Lens' ListInstanceProfilesForRoleResponse Int
lipfrrStatus = lens _lipfrrStatus (\ s a -> s{_lipfrrStatus = a});

-- | A list of instance profiles.
lipfrrInstanceProfiles :: Lens' ListInstanceProfilesForRoleResponse [InstanceProfile]
lipfrrInstanceProfiles = lens _lipfrrInstanceProfiles (\ s a -> s{_lipfrrInstanceProfiles = a});
