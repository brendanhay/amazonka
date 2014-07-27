{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.DetachNetworkInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Detaches a network interface from an instance. Example This example
-- detaches the specified elastic network interface (ENI).
-- https://ec2.amazonaws.com/?Action=DetachNetworkInterface
-- &amp;AttachmentId=eni-attach-d94b09b0 &amp;AUTHPARAMS
-- &lt;DetachNetworkInterfaceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;ce540707-0635-46bc-97da-33a8a362a0e8&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DetachNetworkInterfaceResponse&gt;.
module Network.AWS.EC2.V2014_05_01.DetachNetworkInterface where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DetachNetworkInterface = DetachNetworkInterface
    { _dnirAttachmentId :: Text
      -- ^ The ID of the attachment.
    , _dnirForce :: Maybe Bool
      -- ^ Specifies whether to force a detachment.
    , _dnirDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery DetachNetworkInterface where
    toQuery = genericToQuery def

instance AWSRequest DetachNetworkInterface where
    type Sv DetachNetworkInterface = EC2
    type Rs DetachNetworkInterface = DetachNetworkInterfaceResponse

    request = post "DetachNetworkInterface"
    response _ _ = return (Right DetachNetworkInterfaceResponse)

data DetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse
    deriving (Eq, Show, Generic)
