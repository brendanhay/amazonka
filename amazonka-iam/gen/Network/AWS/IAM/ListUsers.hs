{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListUsers
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the IAM users that have the specified path prefix. If no path
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
    , lursMarker
    , lursIsTruncated
    , lursStatus
    , lursUsers
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
luMaxItems :: Lens' ListUsers (Maybe Natural)
luMaxItems = lens _luMaxItems (\ s a -> s{_luMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
luMarker :: Lens' ListUsers (Maybe Text)
luMarker = lens _luMarker (\ s a -> s{_luMarker = a});

instance AWSPager ListUsers where
        page rq rs
          | stop (rs ^. lursIsTruncated) = Nothing
          | isNothing (rs ^. lursMarker) = Nothing
          | otherwise =
            Just $ rq & luMarker .~ rs ^. lursMarker

instance AWSRequest ListUsers where
        type Sv ListUsers = IAM
        type Rs ListUsers = ListUsersResponse
        request = post
        response
          = receiveXMLWrapper "ListUsersResult"
              (\ s h x ->
                 ListUsersResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "Users" .!@ mempty >>= parseXMLList "member"))

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
-- * 'lursMarker'
--
-- * 'lursIsTruncated'
--
-- * 'lursStatus'
--
-- * 'lursUsers'
data ListUsersResponse = ListUsersResponse'
    { _lursMarker      :: !(Maybe Text)
    , _lursIsTruncated :: !(Maybe Bool)
    , _lursStatus      :: !Int
    , _lursUsers       :: ![User]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListUsersResponse' smart constructor.
listUsersResponse :: Int -> ListUsersResponse
listUsersResponse pStatus_ =
    ListUsersResponse'
    { _lursMarker = Nothing
    , _lursIsTruncated = Nothing
    , _lursStatus = pStatus_
    , _lursUsers = mempty
    }

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lursMarker :: Lens' ListUsersResponse (Maybe Text)
lursMarker = lens _lursMarker (\ s a -> s{_lursMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
lursIsTruncated :: Lens' ListUsersResponse (Maybe Bool)
lursIsTruncated = lens _lursIsTruncated (\ s a -> s{_lursIsTruncated = a});

-- | FIXME: Undocumented member.
lursStatus :: Lens' ListUsersResponse Int
lursStatus = lens _lursStatus (\ s a -> s{_lursStatus = a});

-- | A list of users.
lursUsers :: Lens' ListUsersResponse [User]
lursUsers = lens _lursUsers (\ s a -> s{_lursUsers = a}) . _Coerce;
