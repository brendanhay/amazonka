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
-- Module      : Network.AWS.IAM.ListGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the groups that have the specified path prefix.
--
-- You can paginate the results using the 'MaxItems' and 'Marker'
-- parameters.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroups.html AWS API Reference> for ListGroups.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListGroups
    (
    -- * Creating a Request
      listGroups
    , ListGroups
    -- * Request Lenses
    , lgPathPrefix
    , lgMarker
    , lgMaxItems

    -- * Destructuring the Response
    , listGroupsResponse
    , ListGroupsResponse
    -- * Response Lenses
    , lgrsMarker
    , lgrsIsTruncated
    , lgrsResponseStatus
    , lgrsGroups
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listGroups' smart constructor.
data ListGroups = ListGroups'
    { _lgPathPrefix :: !(Maybe Text)
    , _lgMarker     :: !(Maybe Text)
    , _lgMaxItems   :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgPathPrefix'
--
-- * 'lgMarker'
--
-- * 'lgMaxItems'
listGroups
    :: ListGroups
listGroups =
    ListGroups'
    { _lgPathPrefix = Nothing
    , _lgMarker = Nothing
    , _lgMaxItems = Nothing
    }

-- | The path prefix for filtering the results. For example, the prefix
-- '\/division_abc\/subdivision_xyz\/' gets all groups whose path starts
-- with '\/division_abc\/subdivision_xyz\/'.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/), listing all groups.
lgPathPrefix :: Lens' ListGroups (Maybe Text)
lgPathPrefix = lens _lgPathPrefix (\ s a -> s{_lgPathPrefix = a});

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the 'Marker' element in the response you just received.
lgMarker :: Lens' ListGroups (Maybe Text)
lgMarker = lens _lgMarker (\ s a -> s{_lgMarker = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lgMaxItems :: Lens' ListGroups (Maybe Natural)
lgMaxItems = lens _lgMaxItems (\ s a -> s{_lgMaxItems = a}) . mapping _Nat;

instance AWSPager ListGroups where
        page rq rs
          | stop (rs ^. lgrsMarker) = Nothing
          | stop (rs ^. lgrsGroups) = Nothing
          | otherwise =
            Just $ rq & lgMarker .~ rs ^. lgrsMarker

instance AWSRequest ListGroups where
        type Rs ListGroups = ListGroupsResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "ListGroupsResult"
              (\ s h x ->
                 ListGroupsResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "Groups" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListGroups where
        toHeaders = const mempty

instance ToPath ListGroups where
        toPath = const "/"

instance ToQuery ListGroups where
        toQuery ListGroups'{..}
          = mconcat
              ["Action" =: ("ListGroups" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _lgPathPrefix, "Marker" =: _lgMarker,
               "MaxItems" =: _lgMaxItems]

-- | Contains the response to a successful ListGroups request.
--
-- /See:/ 'listGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
    { _lgrsMarker         :: !(Maybe Text)
    , _lgrsIsTruncated    :: !(Maybe Bool)
    , _lgrsResponseStatus :: !Int
    , _lgrsGroups         :: ![Group]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgrsMarker'
--
-- * 'lgrsIsTruncated'
--
-- * 'lgrsResponseStatus'
--
-- * 'lgrsGroups'
listGroupsResponse
    :: Int -- ^ 'lgrsResponseStatus'
    -> ListGroupsResponse
listGroupsResponse pResponseStatus_ =
    ListGroupsResponse'
    { _lgrsMarker = Nothing
    , _lgrsIsTruncated = Nothing
    , _lgrsResponseStatus = pResponseStatus_
    , _lgrsGroups = mempty
    }

-- | When 'IsTruncated' is 'true', this element is present and contains the
-- value to use for the 'Marker' parameter in a subsequent pagination
-- request.
lgrsMarker :: Lens' ListGroupsResponse (Maybe Text)
lgrsMarker = lens _lgrsMarker (\ s a -> s{_lgrsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more items.
lgrsIsTruncated :: Lens' ListGroupsResponse (Maybe Bool)
lgrsIsTruncated = lens _lgrsIsTruncated (\ s a -> s{_lgrsIsTruncated = a});

-- | The response status code.
lgrsResponseStatus :: Lens' ListGroupsResponse Int
lgrsResponseStatus = lens _lgrsResponseStatus (\ s a -> s{_lgrsResponseStatus = a});

-- | A list of groups.
lgrsGroups :: Lens' ListGroupsResponse [Group]
lgrsGroups = lens _lgrsGroups (\ s a -> s{_lgrsGroups = a}) . _Coerce;
