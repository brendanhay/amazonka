{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DisassociateAddress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Disassociates an Elastic IP address from the instance or network interface
-- it's associated with. This is an idempotent operation. If you perform the
-- operation more than once, Amazon EC2 doesn't return an error. Example for
-- EC2-Classic This example disassociates the specified Elastic IP address
-- from the instance in EC2-Classic to which it is associated.
-- https://ec2.amazonaws.com/?Action=DisassociateAddress
-- &amp;PublicIp=192.0.2.1 &amp;AUTHPARAMS Example for EC2-VPC This example
-- disassociates the specified Elastic IP address from the instance in a VPC
-- to which it is associated.
-- https://ec2.amazonaws.com/?Action=DisassociateAddress
-- &amp;AssociationId=eipassoc-aa7486c3 &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.DisassociateAddress where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DisassociateAddress' request.
disassociateAddress :: DisassociateAddress
disassociateAddress = DisassociateAddress
    { _datDryRun = Nothing
    , _datAssociationId = Nothing
    , _datPublicIp = Nothing
    }

data DisassociateAddress = DisassociateAddress
    { _datDryRun :: Maybe Bool
      -- ^ 
    , _datAssociationId :: Maybe Text
      -- ^ [EC2-VPC] The association ID.
    , _datPublicIp :: Maybe Text
      -- ^ [EC2-Classic] The Elastic IP address.
    } deriving (Show, Generic)

makeLenses ''DisassociateAddress

instance ToQuery DisassociateAddress where
    toQuery = genericQuery def

data DisassociateAddressResponse = DisassociateAddressResponse
    deriving (Eq, Show, Generic)

makeLenses ''DisassociateAddressResponse

instance AWSRequest DisassociateAddress where
    type Sv DisassociateAddress = EC2
    type Rs DisassociateAddress = DisassociateAddressResponse

    request = post "DisassociateAddress"
    response _ = nullaryResponse DisassociateAddressResponse
