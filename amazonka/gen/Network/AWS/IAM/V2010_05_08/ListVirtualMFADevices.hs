{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ListVirtualMFADevices
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
module Network.AWS.IAM.V2010_05_08.ListVirtualMFADevices where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListVirtualMFADevices' request.
listVirtualMFADevices :: ListVirtualMFADevices
listVirtualMFADevices = ListVirtualMFADevices
    { _lvmfadrAssignmentStatus = Nothing
    , _lvmfadrMarker = Nothing
    , _lvmfadrMaxItems = Nothing
    }

data ListVirtualMFADevices = ListVirtualMFADevices
    { _lvmfadrAssignmentStatus :: Maybe AssignmentStatusType
      -- ^ The status (unassigned or assigned) of the devices to list. If
      -- you do not specify an AssignmentStatus, the action defaults to
      -- Any which lists both assigned and unassigned virtual MFA devices.
    , _lvmfadrMarker :: Maybe Text
      -- ^ Use this parameter only when paginating results, and only in a
      -- subsequent request after you've received a response where the
      -- results are truncated. Set it to the value of the Marker element
      -- in the response you just received.
    , _lvmfadrMaxItems :: Maybe Integer
      -- ^ Use this parameter only when paginating results to indicate the
      -- maximum number of user names you want in the response. If there
      -- are additional user names beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    } deriving (Generic)

makeLenses ''ListVirtualMFADevices

instance ToQuery ListVirtualMFADevices where
    toQuery = genericToQuery def

data ListVirtualMFADevicesResponse = ListVirtualMFADevicesResponse
    { _lvmfadsIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more items to list. If
      -- your results were truncated, you can make a subsequent pagination
      -- request using the Marker request parameter to retrieve more items
      -- the list.
    , _lvmfadsVirtualMFADevices :: [VirtualMFADevice]
    , _lvmfadsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Generic)

makeLenses ''ListVirtualMFADevicesResponse

instance FromXML ListVirtualMFADevicesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListVirtualMFADevices where
    type Sv ListVirtualMFADevices = IAM
    type Rs ListVirtualMFADevices = ListVirtualMFADevicesResponse

    request = post "ListVirtualMFADevices"
    response _ = xmlResponse

instance AWSPager ListVirtualMFADevices where
    next rq rs
        | not (_lvmfadsIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _lvmfadrMarker = _lvmfadsMarker rs
            }
