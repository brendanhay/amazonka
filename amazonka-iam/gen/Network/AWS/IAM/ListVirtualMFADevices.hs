{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.ListVirtualMFADevices
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

-- | Lists the virtual MFA devices under the AWS account by assignment
-- status. If you do not specify an assignment status, the action returns a
-- list of all virtual MFA devices. Assignment status can be @Assigned@,
-- @Unassigned@, or @Any@.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListVirtualMFADevices.html>
module Network.AWS.IAM.ListVirtualMFADevices
    (
    -- * Request
      ListVirtualMFADevices
    -- ** Request constructor
    , listVirtualMFADevices
    -- ** Request lenses
    , lvmdAssignmentStatus
    , lvmdMaxItems
    , lvmdMarker

    -- * Response
    , ListVirtualMFADevicesResponse
    -- ** Response constructor
    , listVirtualMFADevicesResponse
    -- ** Response lenses
    , lvmdrMarker
    , lvmdrIsTruncated
    , lvmdrVirtualMFADevices
    , lvmdrStatus
    ) where

import           Network.AWS.IAM.Types
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
    } deriving (Eq,Read,Show)

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

-- | Use this parameter only when paginating results to indicate the maximum
-- number of MFA devices you want in the response. If there are additional
-- MFA devices beyond the maximum you specify, the @IsTruncated@ response
-- element is @true@. This parameter is optional. If you do not include it,
-- it defaults to 100.
lvmdMaxItems :: Lens' ListVirtualMFADevices (Maybe Natural)
lvmdMaxItems = lens _lvmdMaxItems (\ s a -> s{_lvmdMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results, and only in a
-- subsequent request after you\'ve received a response where the results
-- are truncated. Set it to the value of the @Marker@ element in the
-- response you just received.
lvmdMarker :: Lens' ListVirtualMFADevices (Maybe Text)
lvmdMarker = lens _lvmdMarker (\ s a -> s{_lvmdMarker = a});

instance AWSPager ListVirtualMFADevices where
        page rq rs
          | stop (rs ^. lvmdrIsTruncated) = Nothing
          | isNothing (rs ^. lvmdrMarker) = Nothing
          | otherwise =
            Just $ rq & lvmdMarker .~ rs ^. lvmdrMarker

instance AWSRequest ListVirtualMFADevices where
        type Sv ListVirtualMFADevices = IAM
        type Rs ListVirtualMFADevices =
             ListVirtualMFADevicesResponse
        request = post
        response
          = receiveXMLWrapper "ListVirtualMFADevicesResult"
              (\ s h x ->
                 ListVirtualMFADevicesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (x .@? "VirtualMFADevices" .!@ mempty >>=
                        parseXMLList "member")
                     <*> (pure s))

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
-- * 'lvmdrMarker'
--
-- * 'lvmdrIsTruncated'
--
-- * 'lvmdrVirtualMFADevices'
--
-- * 'lvmdrStatus'
data ListVirtualMFADevicesResponse = ListVirtualMFADevicesResponse'
    { _lvmdrMarker            :: !(Maybe Text)
    , _lvmdrIsTruncated       :: !(Maybe Bool)
    , _lvmdrVirtualMFADevices :: ![VirtualMFADevice]
    , _lvmdrStatus            :: !Status
    } deriving (Eq,Show)

-- | 'ListVirtualMFADevicesResponse' smart constructor.
listVirtualMFADevicesResponse :: Status -> ListVirtualMFADevicesResponse
listVirtualMFADevicesResponse pStatus =
    ListVirtualMFADevicesResponse'
    { _lvmdrMarker = Nothing
    , _lvmdrIsTruncated = Nothing
    , _lvmdrVirtualMFADevices = mempty
    , _lvmdrStatus = pStatus
    }

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lvmdrMarker :: Lens' ListVirtualMFADevicesResponse (Maybe Text)
lvmdrMarker = lens _lvmdrMarker (\ s a -> s{_lvmdrMarker = a});

-- | A flag that indicates whether there are more items to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items the list.
lvmdrIsTruncated :: Lens' ListVirtualMFADevicesResponse (Maybe Bool)
lvmdrIsTruncated = lens _lvmdrIsTruncated (\ s a -> s{_lvmdrIsTruncated = a});

-- | The list of virtual MFA devices in the current account that match the
-- @AssignmentStatus@ value that was passed in the request.
lvmdrVirtualMFADevices :: Lens' ListVirtualMFADevicesResponse [VirtualMFADevice]
lvmdrVirtualMFADevices = lens _lvmdrVirtualMFADevices (\ s a -> s{_lvmdrVirtualMFADevices = a});

-- | FIXME: Undocumented member.
lvmdrStatus :: Lens' ListVirtualMFADevicesResponse Status
lvmdrStatus = lens _lvmdrStatus (\ s a -> s{_lvmdrStatus = a});
