{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ListMFADevices
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the MFA devices. If the request includes the user name, then this
-- action lists all the MFA devices associated with the specified user name.
-- If you do not specify a user name, IAM determines the user name implicitly
-- based on the AWS access key ID signing the request. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListMFADevices &UserName=Bob &AUTHPARAMS
-- Bob R1234 false 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.ListMFADevices where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListMFADevices' request.
listMFADevices :: ListMFADevices
listMFADevices = ListMFADevices
    { _lmfadrUserName = Nothing
    , _lmfadrMarker = Nothing
    , _lmfadrMaxItems = Nothing
    }

data ListMFADevices = ListMFADevices
    { _lmfadrUserName :: Maybe Text
      -- ^ Name of the user whose MFA devices you want to list.
    , _lmfadrMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , _lmfadrMaxItems :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of MFA devices you want in the response. If there are
      -- additional MFA devices beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    } deriving (Show, Generic)

makeLenses ''ListMFADevices

instance ToQuery ListMFADevices where
    toQuery = genericQuery def

data ListMFADevicesResponse = ListMFADevicesResponse
    { _lmfadsIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more MFA devices to list.
      -- If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more MFA devices in the list.
    , _lmfadsMFADevices :: [MFADevice]
      -- ^ A list of MFA devices.
    , _lmfadsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Show, Generic)

makeLenses ''ListMFADevicesResponse

instance FromXML ListMFADevicesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListMFADevices where
    type Sv ListMFADevices = IAM
    type Rs ListMFADevices = ListMFADevicesResponse

    request = post "ListMFADevices"
    response _ = xmlResponse

instance AWSPager ListMFADevices where
    next rq rs
        | not (_lmfadsIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _lmfadrMarker = _lmfadsMarker rs
            }
