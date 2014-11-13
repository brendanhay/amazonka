{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.IAM.ListVirtualMFADevices
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the virtual MFA devices under the AWS account by assignment status.
-- If you do not specify an assignment status, the action returns a list of
-- all virtual MFA devices. Assignment status can be Assigned, Unassigned, or
-- Any. You can paginate the results using the MaxItems and Marker parameters.
module Network.AWS.IAM.ListVirtualMFADevices
    (
    -- * Request
      ListVirtualMFADevices
    -- ** Request constructor
    , listVirtualMFADevices
    -- ** Request lenses
    , lvmfadAssignmentStatus
    , lvmfadMarker
    , lvmfadMaxItems

    -- * Response
    , ListVirtualMFADevicesResponse
    -- ** Response constructor
    , listVirtualMFADevicesResponse
    -- ** Response lenses
    , lvmfadrIsTruncated
    , lvmfadrMarker
    , lvmfadrVirtualMFADevices
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ListVirtualMFADevices = ListVirtualMFADevices
    { _lvmfadAssignmentStatus :: Maybe Text
    , _lvmfadMarker           :: Maybe Text
    , _lvmfadMaxItems         :: Maybe Natural
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListVirtualMFADevices' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvmfadAssignmentStatus' @::@ 'Maybe' 'Text'
--
-- * 'lvmfadMarker' @::@ 'Maybe' 'Text'
--
-- * 'lvmfadMaxItems' @::@ 'Maybe' 'Natural'
--
listVirtualMFADevices :: ListVirtualMFADevices
listVirtualMFADevices = ListVirtualMFADevices
    { _lvmfadAssignmentStatus = Nothing
    , _lvmfadMarker           = Nothing
    , _lvmfadMaxItems         = Nothing
    }

-- | The status (unassigned or assigned) of the devices to list. If you do not
-- specify an AssignmentStatus, the action defaults to Any which lists both
-- assigned and unassigned virtual MFA devices.
lvmfadAssignmentStatus :: Lens' ListVirtualMFADevices (Maybe Text)
lvmfadAssignmentStatus =
    lens _lvmfadAssignmentStatus (\s a -> s { _lvmfadAssignmentStatus = a })

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated.
-- Set it to the value of the Marker element in the response you just
-- received.
lvmfadMarker :: Lens' ListVirtualMFADevices (Maybe Text)
lvmfadMarker = lens _lvmfadMarker (\s a -> s { _lvmfadMarker = a })

-- | Use this parameter only when paginating results to indicate the maximum
-- number of MFA devices you want in the response. If there are additional
-- MFA devices beyond the maximum you specify, the IsTruncated response
-- element is true. This parameter is optional. If you do not include it, it
-- defaults to 100.
lvmfadMaxItems :: Lens' ListVirtualMFADevices (Maybe Natural)
lvmfadMaxItems = lens _lvmfadMaxItems (\s a -> s { _lvmfadMaxItems = a })

instance ToQuery ListVirtualMFADevices

instance ToPath ListVirtualMFADevices where
    toPath = const "/"

data ListVirtualMFADevicesResponse = ListVirtualMFADevicesResponse
    { _lvmfadrIsTruncated       :: Maybe Bool
    , _lvmfadrMarker            :: Maybe Text
    , _lvmfadrVirtualMFADevices :: [VirtualMFADevice]
    } deriving (Eq, Show, Generic)

-- | 'ListVirtualMFADevicesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvmfadrIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lvmfadrMarker' @::@ 'Maybe' 'Text'
--
-- * 'lvmfadrVirtualMFADevices' @::@ ['VirtualMFADevice']
--
listVirtualMFADevicesResponse :: ListVirtualMFADevicesResponse
listVirtualMFADevicesResponse = ListVirtualMFADevicesResponse
    { _lvmfadrVirtualMFADevices = mempty
    , _lvmfadrIsTruncated       = Nothing
    , _lvmfadrMarker            = Nothing
    }

-- | A flag that indicates whether there are more items to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the Marker request parameter to retrieve more items the list.
lvmfadrIsTruncated :: Lens' ListVirtualMFADevicesResponse (Maybe Bool)
lvmfadrIsTruncated =
    lens _lvmfadrIsTruncated (\s a -> s { _lvmfadrIsTruncated = a })

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lvmfadrMarker :: Lens' ListVirtualMFADevicesResponse (Maybe Text)
lvmfadrMarker = lens _lvmfadrMarker (\s a -> s { _lvmfadrMarker = a })

-- | The list of virtual MFA devices in the current account that match the
-- AssignmentStatus value that was passed in the request.
lvmfadrVirtualMFADevices :: Lens' ListVirtualMFADevicesResponse [VirtualMFADevice]
lvmfadrVirtualMFADevices =
    lens _lvmfadrVirtualMFADevices
        (\s a -> s { _lvmfadrVirtualMFADevices = a })

instance AWSRequest ListVirtualMFADevices where
    type Sv ListVirtualMFADevices = IAM
    type Rs ListVirtualMFADevices = ListVirtualMFADevicesResponse

    request  = post "ListVirtualMFADevices"
    response = xmlResponse $ \h x -> ListVirtualMFADevicesResponse
        <$> x %| "IsTruncated"
        <*> x %| "Marker"
        <*> x %| "VirtualMFADevices"
