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
-- Module      : Network.AWS.IAM.ListVirtualMFADevices
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the virtual MFA devices under the AWS account by assignment
-- status. If you do not specify an assignment status, the action returns a
-- list of all virtual MFA devices. Assignment status can be 'Assigned',
-- 'Unassigned', or 'Any'.
--
-- You can paginate the results using the 'MaxItems' and 'Marker'
-- parameters.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListVirtualMFADevices.html AWS API Reference> for ListVirtualMFADevices.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListVirtualMFADevices
    (
    -- * Creating a Request
      listVirtualMFADevices
    , ListVirtualMFADevices
    -- * Request Lenses
    , lvmdAssignmentStatus
    , lvmdMarker
    , lvmdMaxItems

    -- * Destructuring the Response
    , listVirtualMFADevicesResponse
    , ListVirtualMFADevicesResponse
    -- * Response Lenses
    , lvmdrsMarker
    , lvmdrsIsTruncated
    , lvmdrsResponseStatus
    , lvmdrsVirtualMFADevices
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listVirtualMFADevices' smart constructor.
data ListVirtualMFADevices = ListVirtualMFADevices'
    { _lvmdAssignmentStatus :: !(Maybe AssignmentStatusType)
    , _lvmdMarker           :: !(Maybe Text)
    , _lvmdMaxItems         :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListVirtualMFADevices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvmdAssignmentStatus'
--
-- * 'lvmdMarker'
--
-- * 'lvmdMaxItems'
listVirtualMFADevices
    :: ListVirtualMFADevices
listVirtualMFADevices =
    ListVirtualMFADevices'
    { _lvmdAssignmentStatus = Nothing
    , _lvmdMarker = Nothing
    , _lvmdMaxItems = Nothing
    }

-- | The status (unassigned or assigned) of the devices to list. If you do
-- not specify an 'AssignmentStatus', the action defaults to 'Any' which
-- lists both assigned and unassigned virtual MFA devices.
lvmdAssignmentStatus :: Lens' ListVirtualMFADevices (Maybe AssignmentStatusType)
lvmdAssignmentStatus = lens _lvmdAssignmentStatus (\ s a -> s{_lvmdAssignmentStatus = a});

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the 'Marker' element in the response you just received.
lvmdMarker :: Lens' ListVirtualMFADevices (Maybe Text)
lvmdMarker = lens _lvmdMarker (\ s a -> s{_lvmdMarker = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lvmdMaxItems :: Lens' ListVirtualMFADevices (Maybe Natural)
lvmdMaxItems = lens _lvmdMaxItems (\ s a -> s{_lvmdMaxItems = a}) . mapping _Nat;

instance AWSPager ListVirtualMFADevices where
        page rq rs
          | stop (rs ^. lvmdrsMarker) = Nothing
          | stop (rs ^. lvmdrsVirtualMFADevices) = Nothing
          | otherwise =
            Just $ rq & lvmdMarker .~ rs ^. lvmdrsMarker

instance AWSRequest ListVirtualMFADevices where
        type Rs ListVirtualMFADevices =
             ListVirtualMFADevicesResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "ListVirtualMFADevicesResult"
              (\ s h x ->
                 ListVirtualMFADevicesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "VirtualMFADevices" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListVirtualMFADevices where
        toHeaders = const mempty

instance ToPath ListVirtualMFADevices where
        toPath = const "/"

instance ToQuery ListVirtualMFADevices where
        toQuery ListVirtualMFADevices'{..}
          = mconcat
              ["Action" =: ("ListVirtualMFADevices" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "AssignmentStatus" =: _lvmdAssignmentStatus,
               "Marker" =: _lvmdMarker, "MaxItems" =: _lvmdMaxItems]

-- | Contains the response to a successful ListVirtualMFADevices request.
--
-- /See:/ 'listVirtualMFADevicesResponse' smart constructor.
data ListVirtualMFADevicesResponse = ListVirtualMFADevicesResponse'
    { _lvmdrsMarker            :: !(Maybe Text)
    , _lvmdrsIsTruncated       :: !(Maybe Bool)
    , _lvmdrsResponseStatus    :: !Int
    , _lvmdrsVirtualMFADevices :: ![VirtualMFADevice]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListVirtualMFADevicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvmdrsMarker'
--
-- * 'lvmdrsIsTruncated'
--
-- * 'lvmdrsResponseStatus'
--
-- * 'lvmdrsVirtualMFADevices'
listVirtualMFADevicesResponse
    :: Int -- ^ 'lvmdrsResponseStatus'
    -> ListVirtualMFADevicesResponse
listVirtualMFADevicesResponse pResponseStatus_ =
    ListVirtualMFADevicesResponse'
    { _lvmdrsMarker = Nothing
    , _lvmdrsIsTruncated = Nothing
    , _lvmdrsResponseStatus = pResponseStatus_
    , _lvmdrsVirtualMFADevices = mempty
    }

-- | When 'IsTruncated' is 'true', this element is present and contains the
-- value to use for the 'Marker' parameter in a subsequent pagination
-- request.
lvmdrsMarker :: Lens' ListVirtualMFADevicesResponse (Maybe Text)
lvmdrsMarker = lens _lvmdrsMarker (\ s a -> s{_lvmdrsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more items.
lvmdrsIsTruncated :: Lens' ListVirtualMFADevicesResponse (Maybe Bool)
lvmdrsIsTruncated = lens _lvmdrsIsTruncated (\ s a -> s{_lvmdrsIsTruncated = a});

-- | The response status code.
lvmdrsResponseStatus :: Lens' ListVirtualMFADevicesResponse Int
lvmdrsResponseStatus = lens _lvmdrsResponseStatus (\ s a -> s{_lvmdrsResponseStatus = a});

-- | The list of virtual MFA devices in the current account that match the
-- 'AssignmentStatus' value that was passed in the request.
lvmdrsVirtualMFADevices :: Lens' ListVirtualMFADevicesResponse [VirtualMFADevice]
lvmdrsVirtualMFADevices = lens _lvmdrsVirtualMFADevices (\ s a -> s{_lvmdrsVirtualMFADevices = a}) . _Coerce;
