{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.AllocateAddress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Acquires an Elastic IP address. An Elastic IP address is for use either in
-- the EC2-Classic platform or in a VPC. For more information, see Elastic IP
-- Addresses in the Amazon Elastic Compute Cloud User Guide. Example for
-- EC2-Classic This example request allocates an Elastic IP address for use
-- with instances in EC2-Classic.
-- https://ec2.amazonaws.com/?Action=AllocateAddress &amp;AUTHPARAMS
-- &lt;AllocateAddressResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;publicIp&gt;192.0.2.1&lt;/publicIp&gt;
-- &lt;domain&gt;standard&lt;/domain&gt; &lt;/AllocateAddressResponse&gt;
-- Example for EC2-VPC This example request allocates an Elastic IP address
-- for use with instances in a VPC.
-- https://ec2.amazonaws.com/?Action=AllocateAddress Domain=vpc
-- &amp;AUTHPARAMS &lt;AllocateAddressResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;publicIp&gt;198.51.100.1&lt;/publicIp&gt;
-- &lt;domain&gt;vpc&lt;/domain&gt;
-- &lt;allocationId&gt;eipalloc-5723d13e&lt;/allocationId&gt;
-- &lt;/AllocateAddressResponse&gt;.
module Network.AWS.EC2.V2014_05_01.AllocateAddress where

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

data AllocateAddress = AllocateAddress
    { _aarDryRun :: Maybe Bool
      -- ^ 
    , _aarDomain :: Maybe DomainType
      -- ^ Set to vpc to allocate the address for use with instances in a
      -- VPC. Default: The address is for use with instances in
      -- EC2-Classic.
    } deriving (Generic)

instance ToQuery AllocateAddress where
    toQuery = genericToQuery def

instance AWSRequest AllocateAddress where
    type Sv AllocateAddress = EC2
    type Rs AllocateAddress = AllocateAddressResponse

    request = post "AllocateAddress"
    response _ = xmlResponse

data AllocateAddressResponse = AllocateAddressResponse
    { _aawDomain :: Maybe DomainType
      -- ^ Indicates whether this Elastic IP address is for use with
      -- instances in EC2-Classic (standard) or instances in a VPC (vpc).
    , _aawAllocationId :: Maybe Text
      -- ^ [EC2-VPC] The ID that AWS assigns to represent the allocation of
      -- the Elastic IP address for use with instances in a VPC.
    , _aawPublicIp :: Maybe Text
      -- ^ The Elastic IP address.
    } deriving (Generic)

instance FromXML AllocateAddressResponse where
    fromXMLOptions = xmlOptions
