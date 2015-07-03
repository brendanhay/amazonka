{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.ListRoles
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

-- | Lists the roles that have the specified path prefix. If there are none,
-- the action returns an empty list. For more information about roles, go
-- to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles>.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListRoles.html>
module Network.AWS.IAM.ListRoles
    (
    -- * Request
      ListRoles
    -- ** Request constructor
    , listRoles
    -- ** Request lenses
    , lrPathPrefix
    , lrMaxItems
    , lrMarker

    -- * Response
    , ListRolesResponse
    -- ** Response constructor
    , listRolesResponse
    -- ** Response lenses
    , lrrMarker
    , lrrIsTruncated
    , lrrStatus
    , lrrRoles
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listRoles' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrPathPrefix'
--
-- * 'lrMaxItems'
--
-- * 'lrMarker'
data ListRoles = ListRoles'
    { _lrPathPrefix :: !(Maybe Text)
    , _lrMaxItems   :: !(Maybe Nat)
    , _lrMarker     :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ListRoles' smart constructor.
listRoles :: ListRoles
listRoles =
    ListRoles'
    { _lrPathPrefix = Nothing
    , _lrMaxItems = Nothing
    , _lrMarker = Nothing
    }

-- | The path prefix for filtering the results. For example, the prefix
-- @\/application_abc\/component_xyz\/@ gets all roles whose path starts
-- with @\/application_abc\/component_xyz\/@.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/), listing all roles.
lrPathPrefix :: Lens' ListRoles (Maybe Text)
lrPathPrefix = lens _lrPathPrefix (\ s a -> s{_lrPathPrefix = a});

-- | Use this parameter only when paginating results to indicate the maximum
-- number of roles you want in the response. If there are additional roles
-- beyond the maximum you specify, the @IsTruncated@ response element is
-- @true@. This parameter is optional. If you do not include it, it
-- defaults to 100.
lrMaxItems :: Lens' ListRoles (Maybe Natural)
lrMaxItems = lens _lrMaxItems (\ s a -> s{_lrMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results, and only in a
-- subsequent request after you\'ve received a response where the results
-- are truncated. Set it to the value of the @Marker@ element in the
-- response you just received.
lrMarker :: Lens' ListRoles (Maybe Text)
lrMarker = lens _lrMarker (\ s a -> s{_lrMarker = a});

instance AWSPager ListRoles where
        page rq rs
          | stop (rs ^. lrrIsTruncated) = Nothing
          | isNothing (rs ^. lrrMarker) = Nothing
          | otherwise = Just $ rq & lrMarker .~ rs ^. lrrMarker

instance AWSRequest ListRoles where
        type Sv ListRoles = IAM
        type Rs ListRoles = ListRolesResponse
        request = post
        response
          = receiveXMLWrapper "ListRolesResult"
              (\ s h x ->
                 ListRolesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "Roles" .!@ mempty >>= parseXMLList "member"))

instance ToHeaders ListRoles where
        toHeaders = const mempty

instance ToPath ListRoles where
        toPath = const "/"

instance ToQuery ListRoles where
        toQuery ListRoles'{..}
          = mconcat
              ["Action" =: ("ListRoles" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _lrPathPrefix,
               "MaxItems" =: _lrMaxItems, "Marker" =: _lrMarker]

-- | Contains the response to a successful ListRoles request.
--
-- /See:/ 'listRolesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrrMarker'
--
-- * 'lrrIsTruncated'
--
-- * 'lrrStatus'
--
-- * 'lrrRoles'
data ListRolesResponse = ListRolesResponse'
    { _lrrMarker      :: !(Maybe Text)
    , _lrrIsTruncated :: !(Maybe Bool)
    , _lrrStatus      :: !Int
    , _lrrRoles       :: ![Role]
    } deriving (Eq,Read,Show)

-- | 'ListRolesResponse' smart constructor.
listRolesResponse :: Int -> ListRolesResponse
listRolesResponse pStatus =
    ListRolesResponse'
    { _lrrMarker = Nothing
    , _lrrIsTruncated = Nothing
    , _lrrStatus = pStatus
    , _lrrRoles = mempty
    }

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lrrMarker :: Lens' ListRolesResponse (Maybe Text)
lrrMarker = lens _lrrMarker (\ s a -> s{_lrrMarker = a});

-- | A flag that indicates whether there are more roles to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more roles in the list.
lrrIsTruncated :: Lens' ListRolesResponse (Maybe Bool)
lrrIsTruncated = lens _lrrIsTruncated (\ s a -> s{_lrrIsTruncated = a});

-- | FIXME: Undocumented member.
lrrStatus :: Lens' ListRolesResponse Int
lrrStatus = lens _lrrStatus (\ s a -> s{_lrrStatus = a});

-- | A list of roles.
lrrRoles :: Lens' ListRolesResponse [Role]
lrrRoles = lens _lrrRoles (\ s a -> s{_lrrRoles = a});
