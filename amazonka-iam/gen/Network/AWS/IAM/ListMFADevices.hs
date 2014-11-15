{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.IAM.ListMFADevices
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
module Network.AWS.IAM.ListMFADevices
    (
    -- * Request
      ListMFADevices
    -- ** Request constructor
    , listMFADevices
    -- ** Request lenses
    , lmfadMarker
    , lmfadMaxItems
    , lmfadUserName

    -- * Response
    , ListMFADevicesResponse
    -- ** Response constructor
    , listMFADevicesResponse
    -- ** Response lenses
    , lmfadrIsTruncated
    , lmfadrMFADevices
    , lmfadrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ListMFADevices = ListMFADevices
    { _lmfadMarker   :: Maybe Text
    , _lmfadMaxItems :: Maybe Nat
    , _lmfadUserName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListMFADevices' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmfadMarker' @::@ 'Maybe' 'Text'
--
-- * 'lmfadMaxItems' @::@ 'Maybe' 'Natural'
--
-- * 'lmfadUserName' @::@ 'Maybe' 'Text'
--
listMFADevices :: ListMFADevices
listMFADevices = ListMFADevices
    { _lmfadUserName = Nothing
    , _lmfadMarker   = Nothing
    , _lmfadMaxItems = Nothing
    }

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it
-- to the value of the Marker element in the response you just received.
lmfadMarker :: Lens' ListMFADevices (Maybe Text)
lmfadMarker = lens _lmfadMarker (\s a -> s { _lmfadMarker = a })

-- | Use this only when paginating results to indicate the maximum number of
-- MFA devices you want in the response. If there are additional MFA devices
-- beyond the maximum you specify, the IsTruncated response element is true.
-- This parameter is optional. If you do not include it, it defaults to 100.
lmfadMaxItems :: Lens' ListMFADevices (Maybe Natural)
lmfadMaxItems = lens _lmfadMaxItems (\s a -> s { _lmfadMaxItems = a })
    . mapping _Nat

-- | The name of the user whose MFA devices you want to list.
lmfadUserName :: Lens' ListMFADevices (Maybe Text)
lmfadUserName = lens _lmfadUserName (\s a -> s { _lmfadUserName = a })

instance ToQuery ListMFADevices

instance ToPath ListMFADevices where
    toPath = const "/"

data ListMFADevicesResponse = ListMFADevicesResponse
    { _lmfadrIsTruncated :: Maybe Bool
    , _lmfadrMFADevices  :: [MFADevice]
    , _lmfadrMarker      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListMFADevicesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmfadrIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lmfadrMFADevices' @::@ ['MFADevice']
--
-- * 'lmfadrMarker' @::@ 'Maybe' 'Text'
--
listMFADevicesResponse :: ListMFADevicesResponse
listMFADevicesResponse = ListMFADevicesResponse
    { _lmfadrMFADevices  = mempty
    , _lmfadrIsTruncated = Nothing
    , _lmfadrMarker      = Nothing
    }

-- | A flag that indicates whether there are more MFA devices to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the Marker request parameter to retrieve more MFA devices in the
-- list.
lmfadrIsTruncated :: Lens' ListMFADevicesResponse (Maybe Bool)
lmfadrIsTruncated =
    lens _lmfadrIsTruncated (\s a -> s { _lmfadrIsTruncated = a })

-- | A list of MFA devices.
lmfadrMFADevices :: Lens' ListMFADevicesResponse [MFADevice]
lmfadrMFADevices = lens _lmfadrMFADevices (\s a -> s { _lmfadrMFADevices = a })

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lmfadrMarker :: Lens' ListMFADevicesResponse (Maybe Text)
lmfadrMarker = lens _lmfadrMarker (\s a -> s { _lmfadrMarker = a })

instance AWSRequest ListMFADevices where
    type Sv ListMFADevices = IAM
    type Rs ListMFADevices = ListMFADevicesResponse

    request  = post "ListMFADevices"
    response = xmlResponse $ \h x -> ListMFADevicesResponse
        <$> x %| "IsTruncated"
        <*> x %| "MFADevices"
        <*> x %| "Marker"

instance AWSPager ListMFADevices where
    next rq rs
        | not (more (rs ^. lmfadrIsTruncated)) = Nothing
        | otherwise = Just $ rq
            & lmfadMarker .~ rs ^. lmfadrMarker
