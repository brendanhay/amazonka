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

-- Module      : Network.AWS.IAM.ResyncMFADevice
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Synchronizes the specified MFA device with AWS servers. For more
-- information about creating and working with virtual MFA devices, go to
-- Using a Virtual MFA Device in the Using IAM guide.
module Network.AWS.IAM.ResyncMFADevice
    (
    -- * Request
      ResyncMFADevice
    -- ** Request constructor
    , resyncMFADevice
    -- ** Request lenses
    , rmfadAuthenticationCode1
    , rmfadAuthenticationCode2
    , rmfadSerialNumber
    , rmfadUserName

    -- * Response
    , ResyncMFADeviceResponse
    -- ** Response constructor
    , resyncMFADeviceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ResyncMFADevice = ResyncMFADevice
    { _rmfadAuthenticationCode1 :: Text
    , _rmfadAuthenticationCode2 :: Text
    , _rmfadSerialNumber        :: Text
    , _rmfadUserName            :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ResyncMFADevice' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rmfadAuthenticationCode1' @::@ 'Text'
--
-- * 'rmfadAuthenticationCode2' @::@ 'Text'
--
-- * 'rmfadSerialNumber' @::@ 'Text'
--
-- * 'rmfadUserName' @::@ 'Text'
--
resyncMFADevice :: Text -- ^ 'rmfadUserName'
                -> Text -- ^ 'rmfadSerialNumber'
                -> Text -- ^ 'rmfadAuthenticationCode1'
                -> Text -- ^ 'rmfadAuthenticationCode2'
                -> ResyncMFADevice
resyncMFADevice p1 p2 p3 p4 = ResyncMFADevice
    { _rmfadUserName            = p1
    , _rmfadSerialNumber        = p2
    , _rmfadAuthenticationCode1 = p3
    , _rmfadAuthenticationCode2 = p4
    }

-- | An authentication code emitted by the device.
rmfadAuthenticationCode1 :: Lens' ResyncMFADevice Text
rmfadAuthenticationCode1 =
    lens _rmfadAuthenticationCode1
        (\s a -> s { _rmfadAuthenticationCode1 = a })

-- | A subsequent authentication code emitted by the device.
rmfadAuthenticationCode2 :: Lens' ResyncMFADevice Text
rmfadAuthenticationCode2 =
    lens _rmfadAuthenticationCode2
        (\s a -> s { _rmfadAuthenticationCode2 = a })

-- | Serial number that uniquely identifies the MFA device.
rmfadSerialNumber :: Lens' ResyncMFADevice Text
rmfadSerialNumber =
    lens _rmfadSerialNumber (\s a -> s { _rmfadSerialNumber = a })

-- | The name of the user whose MFA device you want to resynchronize.
rmfadUserName :: Lens' ResyncMFADevice Text
rmfadUserName = lens _rmfadUserName (\s a -> s { _rmfadUserName = a })

instance ToQuery ResyncMFADevice

instance ToPath ResyncMFADevice where
    toPath = const "/"

data ResyncMFADeviceResponse = ResyncMFADeviceResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'ResyncMFADeviceResponse' constructor.
resyncMFADeviceResponse :: ResyncMFADeviceResponse
resyncMFADeviceResponse = ResyncMFADeviceResponse

instance AWSRequest ResyncMFADevice where
    type Sv ResyncMFADevice = IAM
    type Rs ResyncMFADevice = ResyncMFADeviceResponse

    request  = post "ResyncMFADevice"
    response = nullaryResponse ResyncMFADeviceResponse
