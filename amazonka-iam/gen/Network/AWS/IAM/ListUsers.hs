{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListUsers
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IAM users that have the specified path prefix. If no path
-- prefix is specified, the action returns all users in the AWS account. If
-- there are none, the action returns an empty list.
--
-- You can paginate the results using the 'MaxItems' and 'Marker'
-- parameters.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListUsers.html AWS API Reference> for ListUsers.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListUsers
    (
    -- * Creating a Request
      listUsers
    , ListUsers
    -- * Request Lenses
    , luPathPrefix
    , luMarker
    , luMaxItems

    -- * Destructuring the Response
    , listUsersResponse
    , ListUsersResponse
    -- * Response Lenses
    , lursMarker
    , lursIsTruncated
    , lursResponseStatus
    , lursUsers
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listUsers' smart constructor.
data ListUsers = ListUsers'
    { _luPathPrefix :: !(Maybe Text)
    , _luMarker     :: !(Maybe Text)
    , _luMaxItems   :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListUsers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luPathPrefix'
--
-- * 'luMarker'
--
-- * 'luMaxItems'
listUsers
    :: ListUsers
listUsers =
    ListUsers'
    { _luPathPrefix = Nothing
    , _luMarker = Nothing
    , _luMaxItems = Nothing
    }

-- | The path prefix for filtering the results. For example:
-- '\/division_abc\/subdivision_xyz\/', which would get all user names
-- whose path starts with '\/division_abc\/subdivision_xyz\/'.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/), listing all user names.
luPathPrefix :: Lens' ListUsers (Maybe Text)
luPathPrefix = lens _luPathPrefix (\ s a -> s{_luPathPrefix = a});

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the 'Marker' element in the response you just received.
luMarker :: Lens' ListUsers (Maybe Text)
luMarker = lens _luMarker (\ s a -> s{_luMarker = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
luMaxItems :: Lens' ListUsers (Maybe Natural)
luMaxItems = lens _luMaxItems (\ s a -> s{_luMaxItems = a}) . mapping _Nat;

instance AWSPager ListUsers where
        page rq rs
          | stop (rs ^. lursMarker) = Nothing
          | stop (rs ^. lursUsers) = Nothing
          | otherwise =
            Just $ rq & luMarker .~ rs ^. lursMarker

instance AWSRequest ListUsers where
        type Rs ListUsers = ListUsersResponse
        request = postQuery iAM
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
               "PathPrefix" =: _luPathPrefix, "Marker" =: _luMarker,
               "MaxItems" =: _luMaxItems]

-- | Contains the response to a successful ListUsers request.
--
-- /See:/ 'listUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
    { _lursMarker         :: !(Maybe Text)
    , _lursIsTruncated    :: !(Maybe Bool)
    , _lursResponseStatus :: !Int
    , _lursUsers          :: ![User]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListUsersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lursMarker'
--
-- * 'lursIsTruncated'
--
-- * 'lursResponseStatus'
--
-- * 'lursUsers'
listUsersResponse
    :: Int -- ^ 'lursResponseStatus'
    -> ListUsersResponse
listUsersResponse pResponseStatus_ =
    ListUsersResponse'
    { _lursMarker = Nothing
    , _lursIsTruncated = Nothing
    , _lursResponseStatus = pResponseStatus_
    , _lursUsers = mempty
    }

-- | When 'IsTruncated' is 'true', this element is present and contains the
-- value to use for the 'Marker' parameter in a subsequent pagination
-- request.
lursMarker :: Lens' ListUsersResponse (Maybe Text)
lursMarker = lens _lursMarker (\ s a -> s{_lursMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more items.
lursIsTruncated :: Lens' ListUsersResponse (Maybe Bool)
lursIsTruncated = lens _lursIsTruncated (\ s a -> s{_lursIsTruncated = a});

-- | The response status code.
lursResponseStatus :: Lens' ListUsersResponse Int
lursResponseStatus = lens _lursResponseStatus (\ s a -> s{_lursResponseStatus = a});

-- | A list of users.
lursUsers :: Lens' ListUsersResponse [User]
lursUsers = lens _lursUsers (\ s a -> s{_lursUsers = a}) . _Coerce;
