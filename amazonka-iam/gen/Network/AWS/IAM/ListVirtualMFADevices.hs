{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM
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
-- the AssignmentStatus is Any --> https://iam.amazonaws.com/
-- ?Action=ListVirtualMFADevices &AssignmentStatus=Any &AUTHPARAMS associated
-- with the account: the first device is unassigned, the second is assigned to
-- the root account, and the third is assigned to a user named ExampleUser
-- under the account. --> false arn:aws:iam::123456789012:mfa/MFAdeviceName
-- arn:aws:iam::123456789012:mfa/RootMFAdeviceName 2011-10-20T20:49:03Z
-- 123456789012 arn:aws:iam::123456789012:root 2009-10-13T22:00:36Z
-- arn:aws:iam:::mfa/ExampleUserMFAdeviceName 2011-10-31T20:45:02Z
-- AIDEXAMPLE4EXAMPLEXYZ / ExampleUser
-- arn:aws:iam::111122223333:user/ExampleUser 2011-07-01T17:23:07Z
-- b61ce1b1-0401-11e1-b2f8-2dEXAMPLEbfc.
module Network.AWS.IAM
    (
    -- * Request
      ListVirtualMFADevices
    -- ** Request constructor
    , mkListVirtualMFADevices
    -- ** Request lenses
    , lvmfadAssignmentStatus
    , lvmfadMarker
    , lvmfadMaxItems

    -- * Response
    , ListVirtualMFADevicesResponse
    -- ** Response constructor
    , mkListVirtualMFADevicesResponse
    -- ** Response lenses
    , lvmfadrVirtualMFADevices
    , lvmfadrIsTruncated
    , lvmfadrMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data ListVirtualMFADevices = ListVirtualMFADevices
    { _lvmfadAssignmentStatus :: Maybe AssignmentStatusType
    , _lvmfadMarker :: Maybe Text
    , _lvmfadMaxItems :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListVirtualMFADevices' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AssignmentStatus ::@ @Maybe AssignmentStatusType@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @MaxItems ::@ @Maybe Integer@
--
mkListVirtualMFADevices :: ListVirtualMFADevices
mkListVirtualMFADevices = ListVirtualMFADevices
    { _lvmfadAssignmentStatus = Nothing
    , _lvmfadMarker = Nothing
    , _lvmfadMaxItems = Nothing
    }

-- | The status (unassigned or assigned) of the devices to list. If you do not
-- specify an AssignmentStatus, the action defaults to Any which lists both
-- assigned and unassigned virtual MFA devices.
lvmfadAssignmentStatus :: Lens' ListVirtualMFADevices (Maybe AssignmentStatusType)
lvmfadAssignmentStatus =
    lens _lvmfadAssignmentStatus (\s a -> s { _lvmfadAssignmentStatus = a })

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated.
-- Set it to the value of the Marker element in the response you just
-- received.
lvmfadMarker :: Lens' ListVirtualMFADevices (Maybe Text)
lvmfadMarker = lens _lvmfadMarker (\s a -> s { _lvmfadMarker = a })

-- | Use this parameter only when paginating results to indicate the maximum
-- number of user names you want in the response. If there are additional user
-- names beyond the maximum you specify, the IsTruncated response element is
-- true. This parameter is optional. If you do not include it, it defaults to
-- 100.
lvmfadMaxItems :: Lens' ListVirtualMFADevices (Maybe Integer)
lvmfadMaxItems = lens _lvmfadMaxItems (\s a -> s { _lvmfadMaxItems = a })

instance ToQuery ListVirtualMFADevices where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the ListVirtualMFADevices
-- action.
data ListVirtualMFADevicesResponse = ListVirtualMFADevicesResponse
    { _lvmfadrVirtualMFADevices :: [VirtualMFADevice]
    , _lvmfadrIsTruncated :: Bool
    , _lvmfadrMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListVirtualMFADevicesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VirtualMFADevices ::@ @[VirtualMFADevice]@
--
-- * @IsTruncated ::@ @Bool@
--
-- * @Marker ::@ @Maybe Text@
--
mkListVirtualMFADevicesResponse :: [VirtualMFADevice] -- ^ 'lvmfadrVirtualMFADevices'
                                -> Bool -- ^ 'lvmfadrIsTruncated'
                                -> ListVirtualMFADevicesResponse
mkListVirtualMFADevicesResponse p1 p2 = ListVirtualMFADevicesResponse
    { _lvmfadrVirtualMFADevices = p1
    , _lvmfadrIsTruncated = p2
    , _lvmfadrMarker = Nothing
    }

lvmfadrVirtualMFADevices :: Lens' ListVirtualMFADevicesResponse [VirtualMFADevice]
lvmfadrVirtualMFADevices =
    lens _lvmfadrVirtualMFADevices
         (\s a -> s { _lvmfadrVirtualMFADevices = a })

-- | A flag that indicates whether there are more items to list. If your results
-- were truncated, you can make a subsequent pagination request using the
-- Marker request parameter to retrieve more items the list.
lvmfadrIsTruncated :: Lens' ListVirtualMFADevicesResponse Bool
lvmfadrIsTruncated =
    lens _lvmfadrIsTruncated (\s a -> s { _lvmfadrIsTruncated = a })

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lvmfadrMarker :: Lens' ListVirtualMFADevicesResponse (Maybe Text)
lvmfadrMarker = lens _lvmfadrMarker (\s a -> s { _lvmfadrMarker = a })

instance FromXML ListVirtualMFADevicesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListVirtualMFADevices where
    type Sv ListVirtualMFADevices = IAM
    type Rs ListVirtualMFADevices = ListVirtualMFADevicesResponse

    request = post "ListVirtualMFADevices"
    response _ = xmlResponse

instance AWSPager ListVirtualMFADevices where
    next rq rs
        | not (rs ^. lvmfadrIsTruncated) = Nothing
        | otherwise = Just $
            rq & lvmfadMarker .~ rs ^. lvmfadrMarker
