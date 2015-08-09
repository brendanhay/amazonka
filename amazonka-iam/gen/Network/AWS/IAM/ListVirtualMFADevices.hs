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
-- list of all virtual MFA devices. Assignment status can be @Assigned@,
-- @Unassigned@, or @Any@.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListVirtualMFADevices.html AWS API Reference> for ListVirtualMFADevices.
module Network.AWS.IAM.ListVirtualMFADevices
    (
    -- * Creating a Request
      ListVirtualMFADevices
    , listVirtualMFADevices
    -- * Request Lenses
    , lvmdAssignmentStatus
    , lvmdMaxItems
    , lvmdMarker

    -- * Destructuring the Response
    , ListVirtualMFADevicesResponse
    , listVirtualMFADevicesResponse
    -- * Response Lenses
    , lvmdrsMarker
    , lvmdrsIsTruncated
    , lvmdrsStatus
    , lvmdrsVirtualMFADevices
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listVirtualMFADevices' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvmdAssignmentStatus'
--
-- * 'lvmdMaxItems'
--
-- * 'lvmdMarker'
data ListVirtualMFADevices = ListVirtualMFADevices'
    { _lvmdAssignmentStatus :: !(Maybe AssignmentStatusType)
    , _lvmdMaxItems         :: !(Maybe Nat)
    , _lvmdMarker           :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListVirtualMFADevices' smart constructor.
listVirtualMFADevices :: ListVirtualMFADevices
listVirtualMFADevices =
    ListVirtualMFADevices'
    { _lvmdAssignmentStatus = Nothing
    , _lvmdMaxItems = Nothing
    , _lvmdMarker = Nothing
    }

-- | The status (unassigned or assigned) of the devices to list. If you do
-- not specify an @AssignmentStatus@, the action defaults to @Any@ which
-- lists both assigned and unassigned virtual MFA devices.
lvmdAssignmentStatus :: Lens' ListVirtualMFADevices (Maybe AssignmentStatusType)
lvmdAssignmentStatus = lens _lvmdAssignmentStatus (\ s a -> s{_lvmdAssignmentStatus = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lvmdMaxItems :: Lens' ListVirtualMFADevices (Maybe Natural)
lvmdMaxItems = lens _lvmdMaxItems (\ s a -> s{_lvmdMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
lvmdMarker :: Lens' ListVirtualMFADevices (Maybe Text)
lvmdMarker = lens _lvmdMarker (\ s a -> s{_lvmdMarker = a});

instance AWSPager ListVirtualMFADevices where
        page rq rs
          | stop (rs ^. lvmdrsIsTruncated) = Nothing
          | isNothing (rs ^. lvmdrsMarker) = Nothing
          | otherwise =
            Just $ rq & lvmdMarker .~ rs ^. lvmdrsMarker

instance AWSRequest ListVirtualMFADevices where
        type Sv ListVirtualMFADevices = IAM
        type Rs ListVirtualMFADevices =
             ListVirtualMFADevicesResponse
        request = postQuery
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
               "MaxItems" =: _lvmdMaxItems, "Marker" =: _lvmdMarker]

-- | Contains the response to a successful ListVirtualMFADevices request.
--
-- /See:/ 'listVirtualMFADevicesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvmdrsMarker'
--
-- * 'lvmdrsIsTruncated'
--
-- * 'lvmdrsStatus'
--
-- * 'lvmdrsVirtualMFADevices'
data ListVirtualMFADevicesResponse = ListVirtualMFADevicesResponse'
    { _lvmdrsMarker            :: !(Maybe Text)
    , _lvmdrsIsTruncated       :: !(Maybe Bool)
    , _lvmdrsStatus            :: !Int
    , _lvmdrsVirtualMFADevices :: ![VirtualMFADevice]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListVirtualMFADevicesResponse' smart constructor.
listVirtualMFADevicesResponse :: Int -> ListVirtualMFADevicesResponse
listVirtualMFADevicesResponse pStatus_ =
    ListVirtualMFADevicesResponse'
    { _lvmdrsMarker = Nothing
    , _lvmdrsIsTruncated = Nothing
    , _lvmdrsStatus = pStatus_
    , _lvmdrsVirtualMFADevices = mempty
    }

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lvmdrsMarker :: Lens' ListVirtualMFADevicesResponse (Maybe Text)
lvmdrsMarker = lens _lvmdrsMarker (\ s a -> s{_lvmdrsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
lvmdrsIsTruncated :: Lens' ListVirtualMFADevicesResponse (Maybe Bool)
lvmdrsIsTruncated = lens _lvmdrsIsTruncated (\ s a -> s{_lvmdrsIsTruncated = a});

-- | Undocumented member.
lvmdrsStatus :: Lens' ListVirtualMFADevicesResponse Int
lvmdrsStatus = lens _lvmdrsStatus (\ s a -> s{_lvmdrsStatus = a});

-- | The list of virtual MFA devices in the current account that match the
-- @AssignmentStatus@ value that was passed in the request.
lvmdrsVirtualMFADevices :: Lens' ListVirtualMFADevicesResponse [VirtualMFADevice]
lvmdrsVirtualMFADevices = lens _lvmdrsVirtualMFADevices (\ s a -> s{_lvmdrsVirtualMFADevices = a}) . _Coerce;
