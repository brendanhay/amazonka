{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.AssociateAddress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Associates an Elastic IP address with an instance or a network interface.
-- An Elastic IP address is for use in either the EC2-Classic platform or in a
-- VPC. For more information, see Elastic IP Addresses in the Amazon Elastic
-- Compute Cloud User Guide. [EC2-Classic, default VPC] If the Elastic IP
-- address is already associated with a different instance, it is
-- disassociated from that instance and associated with the specified
-- instance. [EC2-VPC] If you don't specify a private IP address, the Elastic
-- IP address is associated with the primary IP address. If the Elastic IP
-- address is already associated with a different instance or a network
-- interface, you get an error unless you allow reassociation. This is an
-- idempotent operation. If you perform the operation more than once, Amazon
-- EC2 doesn't return an error. Example for EC2-Classic This example request
-- associates an Elastic IP address with an instance in EC2-Classic.
-- https://ec2.amazonaws.com/?Action=AssociateAddress
-- &amp;InstanceId=i-2ea64347 &amp;PublicIp=192.0.2.1 &amp;AUTHPARAMS
-- &lt;AssociateAddressResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/AssociateAddressResponse&gt; Example
-- for EC2-VPC This example request associates a Elastic IP address with an
-- instance in a VPC. The AllowReassignment parameter allows the Elastic IP
-- address to be associated with the specified instance even if it's already
-- associated with a different instance or a network interface.
-- https://ec2.amazonaws.com/?Action=AssociateAddress
-- &amp;InstanceId=i-4fd2431a &amp;AllocationId=eipalloc-5723d13e
-- &amp;AllowReassignment=true &amp;AUTHPARAMS &lt;AssociateAddressResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;associationId&gt;eipassoc-fc5ca095&lt;/associationId&gt;
-- &lt;/AssociateAddressResponse&gt;.
module Network.AWS.EC2.V2014_06_15.AssociateAddress where

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
import           Network.AWS.EC2.V2014_06_15.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'AssociateAddress' request.
associateAddress :: AssociateAddress
associateAddress = AssociateAddress
    { _aaauAllowReassociation = Nothing
    , _aaauDryRun = Nothing
    , _aaauInstanceId = Nothing
    , _aaauAllocationId = Nothing
    , _aaauNetworkInterfaceId = Nothing
    , _aaauPrivateIpAddress = Nothing
    , _aaauPublicIp = Nothing
    }

data AssociateAddress = AssociateAddress
    { _aaauAllowReassociation :: Maybe Bool
      -- ^ [EC2-VPC] Allows an Elastic IP address that is already associated
      -- with an instance or network interface to be re-associated with
      -- the specified instance or network interface. Otherwise, the
      -- operation fails. Default: false.
    , _aaauDryRun :: Maybe Bool
      -- ^ 
    , _aaauInstanceId :: Maybe Text
      -- ^ The ID of the instance. The operation fails if you specify an
      -- instance ID unless exactly one network interface is attached.
    , _aaauAllocationId :: Maybe Text
      -- ^ [EC2-VPC] The allocation ID. This is required for EC2-VPC.
    , _aaauNetworkInterfaceId :: Maybe Text
      -- ^ [EC2-VPC] The ID of the network interface. If the instance has
      -- more than one network interface, you must specify a network
      -- interface ID.
    , _aaauPrivateIpAddress :: Maybe Text
      -- ^ [EC2-VPC] The primary or secondary private IP address to
      -- associate with the Elastic IP address. If no private IP address
      -- is specified, the Elastic IP address is associated with the
      -- primary private IP address.
    , _aaauPublicIp :: Maybe Text
      -- ^ The Elastic IP address.
    } deriving (Generic)

instance ToQuery AssociateAddress where
    toQuery = genericToQuery def

instance AWSRequest AssociateAddress where
    type Sv AssociateAddress = EC2
    type Rs AssociateAddress = AssociateAddressResponse

    request = post "AssociateAddress"
    response _ = xmlResponse

data AssociateAddressResponse = AssociateAddressResponse
    { _aaavAssociationId :: Maybe Text
      -- ^ [EC2-VPC] The ID that represents the association of the Elastic
      -- IP address with an instance.
    } deriving (Generic)

instance FromXML AssociateAddressResponse where
    fromXMLOptions = xmlOptions
