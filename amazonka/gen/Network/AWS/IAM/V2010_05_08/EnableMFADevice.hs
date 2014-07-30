{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.EnableMFADevice
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables the specified MFA device and associates it with the specified user
-- name. When enabled, the MFA device is required for every subsequent login
-- by the user name associated with the device. https://iam.amazonaws.com/
-- ?Action=EnableMFADevice &UserName=Bob &SerialNumber=R1234
-- &AuthenticationCode1=234567 &AuthenticationCode2=987654 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.EnableMFADevice where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.IAM.V2010_05_08.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data EnableMFADevice = EnableMFADevice
    { _emfadrAuthenticationCode1 :: Text
      -- ^ An authentication code emitted by the device.
    , _emfadrAuthenticationCode2 :: Text
      -- ^ A subsequent authentication code emitted by the device.
    , _emfadrUserName :: Text
      -- ^ Name of the user for whom you want to enable the MFA device.
    , _emfadrSerialNumber :: Text
      -- ^ The serial number that uniquely identifies the MFA device. For
      -- virtual MFA devices, the serial number is the device ARN.
    } deriving (Generic)

instance ToQuery EnableMFADevice where
    toQuery = genericToQuery def

instance AWSRequest EnableMFADevice where
    type Sv EnableMFADevice = IAM
    type Rs EnableMFADevice = EnableMFADeviceResponse

    request = post "EnableMFADevice"
    response _ _ = return (Right EnableMFADeviceResponse)

data EnableMFADeviceResponse = EnableMFADeviceResponse
    deriving (Eq, Show, Generic)
