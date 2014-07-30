{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.DeactivateMFADevice
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deactivates the specified MFA device and removes it from association with
-- the user name for which it was originally enabled.
-- https://iam.amazonaws.com/ ?Action=DeactivateMFADevice &UserName=Bob
-- &SerialNumber=R1234 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.DeactivateMFADevice where

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

data DeactivateMFADevice = DeactivateMFADevice
    { _dmfadrUserName :: Text
      -- ^ Name of the user whose MFA device you want to deactivate.
    , _dmfadrSerialNumber :: Text
      -- ^ The serial number that uniquely identifies the MFA device. For
      -- virtual MFA devices, the serial number is the device ARN.
    } deriving (Generic)

instance ToQuery DeactivateMFADevice where
    toQuery = genericToQuery def

instance AWSRequest DeactivateMFADevice where
    type Sv DeactivateMFADevice = IAM
    type Rs DeactivateMFADevice = DeactivateMFADeviceResponse

    request = post "DeactivateMFADevice"
    response _ _ = return (Right DeactivateMFADeviceResponse)

data DeactivateMFADeviceResponse = DeactivateMFADeviceResponse
    deriving (Eq, Show, Generic)
