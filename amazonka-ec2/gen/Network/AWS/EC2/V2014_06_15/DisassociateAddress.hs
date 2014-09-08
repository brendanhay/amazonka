{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.EC2.V2014_06_15.DisassociateAddress
    (
    -- * Request
      DisassociateAddress
    -- ** Request constructor
    , mkDisassociateAddress
    -- ** Request lenses
    , da1PublicIp
    , da1AssociationId

    -- * Response
    , DisassociateAddressResponse
    -- ** Response constructor
    , mkDisassociateAddressResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data DisassociateAddress = DisassociateAddress
    { _da1PublicIp :: Maybe Text
    , _da1AssociationId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisassociateAddress' request.
mkDisassociateAddress :: DisassociateAddress
mkDisassociateAddress = DisassociateAddress
    { _da1PublicIp = Nothing
    , _da1AssociationId = Nothing
    }

-- | [EC2-Classic] The Elastic IP address.
da1PublicIp :: Lens' DisassociateAddress (Maybe Text)
da1PublicIp = lens _da1PublicIp (\s a -> s { _da1PublicIp = a })

-- | [EC2-VPC] The association ID.
da1AssociationId :: Lens' DisassociateAddress (Maybe Text)
da1AssociationId =
    lens _da1AssociationId (\s a -> s { _da1AssociationId = a })

instance ToQuery DisassociateAddress where
    toQuery = genericQuery def

data DisassociateAddressResponse = DisassociateAddressResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisassociateAddressResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDisassociateAddressResponse :: DisassociateAddressResponse
mkDisassociateAddressResponse = DisassociateAddressResponse

instance AWSRequest DisassociateAddress where
    type Sv DisassociateAddress = EC2
    type Rs DisassociateAddress = DisassociateAddressResponse

    request = post "DisassociateAddress"
    response _ = nullaryResponse DisassociateAddressResponse
