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
-- Module      : Network.AWS.IAM.GetGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of users that are in the specified group. You can
-- paginate the results using the 'MaxItems' and 'Marker' parameters.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetGroup.html AWS API Reference> for GetGroup.
--
-- This operation returns paginated results.
module Network.AWS.IAM.GetGroup
    (
    -- * Creating a Request
      getGroup
    , GetGroup
    -- * Request Lenses
    , ggMarker
    , ggMaxItems
    , ggGroupName

    -- * Destructuring the Response
    , getGroupResponse
    , GetGroupResponse
    -- * Response Lenses
    , ggrsMarker
    , ggrsIsTruncated
    , ggrsResponseStatus
    , ggrsGroup
    , ggrsUsers
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getGroup' smart constructor.
data GetGroup = GetGroup'
    { _ggMarker    :: !(Maybe Text)
    , _ggMaxItems  :: !(Maybe Nat)
    , _ggGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggMarker'
--
-- * 'ggMaxItems'
--
-- * 'ggGroupName'
getGroup
    :: Text -- ^ 'ggGroupName'
    -> GetGroup
getGroup pGroupName_ =
    GetGroup'
    { _ggMarker = Nothing
    , _ggMaxItems = Nothing
    , _ggGroupName = pGroupName_
    }

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the 'Marker' element in the response you just received.
ggMarker :: Lens' GetGroup (Maybe Text)
ggMarker = lens _ggMarker (\ s a -> s{_ggMarker = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
ggMaxItems :: Lens' GetGroup (Maybe Natural)
ggMaxItems = lens _ggMaxItems (\ s a -> s{_ggMaxItems = a}) . mapping _Nat;

-- | The name of the group.
ggGroupName :: Lens' GetGroup Text
ggGroupName = lens _ggGroupName (\ s a -> s{_ggGroupName = a});

instance AWSPager GetGroup where
        page rq rs
          | stop (rs ^. ggrsMarker) = Nothing
          | stop (rs ^. ggrsUsers) = Nothing
          | otherwise =
            Just $ rq & ggMarker .~ rs ^. ggrsMarker

instance AWSRequest GetGroup where
        type Rs GetGroup = GetGroupResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "GetGroupResult"
              (\ s h x ->
                 GetGroupResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*> (x .@ "Group")
                     <*>
                     (x .@? "Users" .!@ mempty >>= parseXMLList "member"))

instance ToHeaders GetGroup where
        toHeaders = const mempty

instance ToPath GetGroup where
        toPath = const "/"

instance ToQuery GetGroup where
        toQuery GetGroup'{..}
          = mconcat
              ["Action" =: ("GetGroup" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Marker" =: _ggMarker, "MaxItems" =: _ggMaxItems,
               "GroupName" =: _ggGroupName]

-- | Contains the response to a successful GetGroup request.
--
-- /See:/ 'getGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
    { _ggrsMarker         :: !(Maybe Text)
    , _ggrsIsTruncated    :: !(Maybe Bool)
    , _ggrsResponseStatus :: !Int
    , _ggrsGroup          :: !Group
    , _ggrsUsers          :: ![User]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggrsMarker'
--
-- * 'ggrsIsTruncated'
--
-- * 'ggrsResponseStatus'
--
-- * 'ggrsGroup'
--
-- * 'ggrsUsers'
getGroupResponse
    :: Int -- ^ 'ggrsResponseStatus'
    -> Group -- ^ 'ggrsGroup'
    -> GetGroupResponse
getGroupResponse pResponseStatus_ pGroup_ =
    GetGroupResponse'
    { _ggrsMarker = Nothing
    , _ggrsIsTruncated = Nothing
    , _ggrsResponseStatus = pResponseStatus_
    , _ggrsGroup = pGroup_
    , _ggrsUsers = mempty
    }

-- | When 'IsTruncated' is 'true', this element is present and contains the
-- value to use for the 'Marker' parameter in a subsequent pagination
-- request.
ggrsMarker :: Lens' GetGroupResponse (Maybe Text)
ggrsMarker = lens _ggrsMarker (\ s a -> s{_ggrsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more items.
ggrsIsTruncated :: Lens' GetGroupResponse (Maybe Bool)
ggrsIsTruncated = lens _ggrsIsTruncated (\ s a -> s{_ggrsIsTruncated = a});

-- | The response status code.
ggrsResponseStatus :: Lens' GetGroupResponse Int
ggrsResponseStatus = lens _ggrsResponseStatus (\ s a -> s{_ggrsResponseStatus = a});

-- | Information about the group.
ggrsGroup :: Lens' GetGroupResponse Group
ggrsGroup = lens _ggrsGroup (\ s a -> s{_ggrsGroup = a});

-- | A list of users in the group.
ggrsUsers :: Lens' GetGroupResponse [User]
ggrsUsers = lens _ggrsUsers (\ s a -> s{_ggrsUsers = a}) . _Coerce;
