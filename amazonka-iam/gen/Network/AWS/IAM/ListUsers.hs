{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.ListUsers
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

-- | Lists the IAM users that have the specified path prefix. If no path
-- prefix is specified, the action returns all users in the AWS account. If
-- there are none, the action returns an empty list.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListUsers.html>
module Network.AWS.IAM.ListUsers
    (
    -- * Request
      ListUsers
    -- ** Request constructor
    , listUsers
    -- ** Request lenses
    , luPathPrefix
    , luMaxItems
    , luMarker

    -- * Response
    , ListUsersResponse
    -- ** Response constructor
    , listUsersResponse
    -- ** Response lenses
    , lurMarker
    , lurIsTruncated
    , lurUsers
    , lurStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listUsers' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'luPathPrefix'
--
-- * 'luMaxItems'
--
-- * 'luMarker'
data ListUsers = ListUsers'
    { _luPathPrefix :: !(Maybe Text)
    , _luMaxItems   :: !(Maybe Nat)
    , _luMarker     :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ListUsers' smart constructor.
listUsers :: ListUsers
listUsers =
    ListUsers'
    { _luPathPrefix = Nothing
    , _luMaxItems = Nothing
    , _luMarker = Nothing
    }

-- | The path prefix for filtering the results. For example:
-- @\/division_abc\/subdivision_xyz\/@, which would get all user names
-- whose path starts with @\/division_abc\/subdivision_xyz\/@.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/), listing all user names.
luPathPrefix :: Lens' ListUsers (Maybe Text)
luPathPrefix = lens _luPathPrefix (\ s a -> s{_luPathPrefix = a});

-- | Use this parameter only when paginating results to indicate the maximum
-- number of user names you want in the response. If there are additional
-- user names beyond the maximum you specify, the @IsTruncated@ response
-- element is @true@. This parameter is optional. If you do not include it,
-- it defaults to 100.
luMaxItems :: Lens' ListUsers (Maybe Natural)
luMaxItems = lens _luMaxItems (\ s a -> s{_luMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results, and only in a
-- subsequent request after you\'ve received a response where the results
-- are truncated. Set it to the value of the @Marker@ element in the
-- response you just received.
luMarker :: Lens' ListUsers (Maybe Text)
luMarker = lens _luMarker (\ s a -> s{_luMarker = a});

instance AWSPager ListUsers where
        page rq rs
          | stop (rs ^. lurIsTruncated) = Nothing
          | isNothing (rs ^. lurMarker) = Nothing
          | otherwise = Just $ rq & luMarker .~ rs ^. lurMarker

instance AWSRequest ListUsers where
        type Sv ListUsers = IAM
        type Rs ListUsers = ListUsersResponse
        request = post
        response
          = receiveXMLWrapper "ListUsersResult"
              (\ s h x ->
                 ListUsersResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (x .@? "Users" .!@ mempty >>= parseXMLList "member")
                     <*> (pure s))

instance ToHeaders ListUsers where
        toHeaders = const mempty

instance ToPath ListUsers where
        toPath = const "/"

instance ToQuery ListUsers where
        toQuery ListUsers'{..}
          = mconcat
              ["Action" =: ("ListUsers" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _luPathPrefix,
               "MaxItems" =: _luMaxItems, "Marker" =: _luMarker]

-- | Contains the response to a successful ListUsers request.
--
-- /See:/ 'listUsersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lurMarker'
--
-- * 'lurIsTruncated'
--
-- * 'lurUsers'
--
-- * 'lurStatus'
data ListUsersResponse = ListUsersResponse'
    { _lurMarker      :: !(Maybe Text)
    , _lurIsTruncated :: !(Maybe Bool)
    , _lurUsers       :: ![User]
    , _lurStatus      :: !Status
    } deriving (Eq,Read,Show)

-- | 'ListUsersResponse' smart constructor.
listUsersResponse :: Status -> ListUsersResponse
listUsersResponse pStatus =
    ListUsersResponse'
    { _lurMarker = Nothing
    , _lurIsTruncated = Nothing
    , _lurUsers = mempty
    , _lurStatus = pStatus
    }

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lurMarker :: Lens' ListUsersResponse (Maybe Text)
lurMarker = lens _lurMarker (\ s a -> s{_lurMarker = a});

-- | A flag that indicates whether there are more user names to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more users in the list.
lurIsTruncated :: Lens' ListUsersResponse (Maybe Bool)
lurIsTruncated = lens _lurIsTruncated (\ s a -> s{_lurIsTruncated = a});

-- | A list of users.
lurUsers :: Lens' ListUsersResponse [User]
lurUsers = lens _lurUsers (\ s a -> s{_lurUsers = a});

-- | FIXME: Undocumented member.
lurStatus :: Lens' ListUsersResponse Status
lurStatus = lens _lurStatus (\ s a -> s{_lurStatus = a});
