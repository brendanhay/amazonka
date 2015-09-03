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
-- Module      : Network.AWS.IAM.ListRoles
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the roles that have the specified path prefix. If there are none,
-- the action returns an empty list. For more information about roles, go
-- to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles>.
--
-- You can paginate the results using the 'MaxItems' and 'Marker'
-- parameters.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListRoles.html AWS API Reference> for ListRoles.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListRoles
    (
    -- * Creating a Request
      listRoles
    , ListRoles
    -- * Request Lenses
    , lrPathPrefix
    , lrMarker
    , lrMaxItems

    -- * Destructuring the Response
    , listRolesResponse
    , ListRolesResponse
    -- * Response Lenses
    , lrrsMarker
    , lrrsIsTruncated
    , lrrsResponseStatus
    , lrrsRoles
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listRoles' smart constructor.
data ListRoles = ListRoles'
    { _lrPathPrefix :: !(Maybe Text)
    , _lrMarker     :: !(Maybe Text)
    , _lrMaxItems   :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListRoles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrPathPrefix'
--
-- * 'lrMarker'
--
-- * 'lrMaxItems'
listRoles
    :: ListRoles
listRoles =
    ListRoles'
    { _lrPathPrefix = Nothing
    , _lrMarker = Nothing
    , _lrMaxItems = Nothing
    }

-- | The path prefix for filtering the results. For example, the prefix
-- '\/application_abc\/component_xyz\/' gets all roles whose path starts
-- with '\/application_abc\/component_xyz\/'.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/), listing all roles.
lrPathPrefix :: Lens' ListRoles (Maybe Text)
lrPathPrefix = lens _lrPathPrefix (\ s a -> s{_lrPathPrefix = a});

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the 'Marker' element in the response you just received.
lrMarker :: Lens' ListRoles (Maybe Text)
lrMarker = lens _lrMarker (\ s a -> s{_lrMarker = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lrMaxItems :: Lens' ListRoles (Maybe Natural)
lrMaxItems = lens _lrMaxItems (\ s a -> s{_lrMaxItems = a}) . mapping _Nat;

instance AWSPager ListRoles where
        page rq rs
          | stop (rs ^. lrrsMarker) = Nothing
          | stop (rs ^. lrrsRoles) = Nothing
          | otherwise =
            Just $ rq & lrMarker .~ rs ^. lrrsMarker

instance AWSRequest ListRoles where
        type Rs ListRoles = ListRolesResponse
        request = postQuery iAM
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
               "PathPrefix" =: _lrPathPrefix, "Marker" =: _lrMarker,
               "MaxItems" =: _lrMaxItems]

-- | Contains the response to a successful ListRoles request.
--
-- /See:/ 'listRolesResponse' smart constructor.
data ListRolesResponse = ListRolesResponse'
    { _lrrsMarker         :: !(Maybe Text)
    , _lrrsIsTruncated    :: !(Maybe Bool)
    , _lrrsResponseStatus :: !Int
    , _lrrsRoles          :: ![Role]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListRolesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrrsMarker'
--
-- * 'lrrsIsTruncated'
--
-- * 'lrrsResponseStatus'
--
-- * 'lrrsRoles'
listRolesResponse
    :: Int -- ^ 'lrrsResponseStatus'
    -> ListRolesResponse
listRolesResponse pResponseStatus_ =
    ListRolesResponse'
    { _lrrsMarker = Nothing
    , _lrrsIsTruncated = Nothing
    , _lrrsResponseStatus = pResponseStatus_
    , _lrrsRoles = mempty
    }

-- | When 'IsTruncated' is 'true', this element is present and contains the
-- value to use for the 'Marker' parameter in a subsequent pagination
-- request.
lrrsMarker :: Lens' ListRolesResponse (Maybe Text)
lrrsMarker = lens _lrrsMarker (\ s a -> s{_lrrsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more items.
lrrsIsTruncated :: Lens' ListRolesResponse (Maybe Bool)
lrrsIsTruncated = lens _lrrsIsTruncated (\ s a -> s{_lrrsIsTruncated = a});

-- | The response status code.
lrrsResponseStatus :: Lens' ListRolesResponse Int
lrrsResponseStatus = lens _lrrsResponseStatus (\ s a -> s{_lrrsResponseStatus = a});

-- | A list of roles.
lrrsRoles :: Lens' ListRolesResponse [Role]
lrrsRoles = lens _lrrsRoles (\ s a -> s{_lrrsRoles = a}) . _Coerce;
