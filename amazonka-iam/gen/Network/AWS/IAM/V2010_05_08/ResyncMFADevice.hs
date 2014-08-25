{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ResyncMFADevice
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Synchronizes the specified MFA device with AWS servers.
-- https://iam.amazonaws.com/ ?Action=ResyncMFADevice &UserName=Bob
-- &SerialNumber=R1234 &AuthenticationCode1=234567 &AuthenticationCode2=987654
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.ResyncMFADevice where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

data ResyncMFADevice = ResyncMFADevice
    { _rmfadrAuthenticationCode1 :: Text
      -- ^ An authentication code emitted by the device.
    , _rmfadrAuthenticationCode2 :: Text
      -- ^ A subsequent authentication code emitted by the device.
    , _rmfadrUserName :: Text
      -- ^ Name of the user whose MFA device you want to resynchronize.
    , _rmfadrSerialNumber :: Text
      -- ^ Serial number that uniquely identifies the MFA device.
    } deriving (Show, Generic)

makeLenses ''ResyncMFADevice

instance ToQuery ResyncMFADevice where
    toQuery = genericQuery def

data ResyncMFADeviceResponse = ResyncMFADeviceResponse
    deriving (Eq, Show, Generic)

makeLenses ''ResyncMFADeviceResponse

instance AWSRequest ResyncMFADevice where
    type Sv ResyncMFADevice = IAM
    type Rs ResyncMFADevice = ResyncMFADeviceResponse

    request = post "ResyncMFADevice"
    response _ = nullaryResponse ResyncMFADeviceResponse
