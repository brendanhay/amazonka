{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.AllocateAddress
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
module Network.AWS.EC2.V2014_06_15.AllocateAddress
    (
    -- * Request
      AllocateAddress
    -- ** Default constructor
    , allocateAddress
    -- ** Accessors and lenses
    , _aarDomain
    , aarDomain

    -- * Response
    , AllocateAddressResponse
    -- ** Accessors and lenses
    , _aasDomain
    , aasDomain
    , _aasPublicIp
    , aasPublicIp
    , _aasAllocationId
    , aasAllocationId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AllocateAddress' request.
allocateAddress :: AllocateAddress
allocateAddress = AllocateAddress
    { _aarDomain = Nothing
    }

data AllocateAddress = AllocateAddress

makeSiglessLenses ''AllocateAddress

instance ToQuery AllocateAddress where
    toQuery = genericQuery def

data AllocateAddressResponse = AllocateAddressResponse
    { _aasDomain :: Maybe DomainType
      -- ^ Indicates whether this Elastic IP address is for use with
      -- instances in EC2-Classic (standard) or instances in a VPC (vpc).
    , _aasPublicIp :: Maybe Text
      -- ^ The Elastic IP address.
    , _aasAllocationId :: Maybe Text
      -- ^ [EC2-VPC] The ID that AWS assigns to represent the allocation of
      -- the Elastic IP address for use with instances in a VPC.
    } deriving (Show, Generic)

makeSiglessLenses ''AllocateAddressResponse

instance FromXML AllocateAddressResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AllocateAddress where
    type Sv AllocateAddress = EC2
    type Rs AllocateAddress = AllocateAddressResponse

    request = post "AllocateAddress"
    response _ = xmlResponse

-- | Set to vpc to allocate the address for use with instances in a VPC.
-- Default: The address is for use with instances in EC2-Classic.
aarDomain :: Lens' AllocateAddress (Maybe DomainType)

-- | Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic (standard) or instances in a VPC (vpc).
aasDomain :: Lens' AllocateAddressResponse (Maybe DomainType)

-- | The Elastic IP address.
aasPublicIp :: Lens' AllocateAddressResponse (Maybe Text)

-- | [EC2-VPC] The ID that AWS assigns to represent the allocation of the
-- Elastic IP address for use with instances in a VPC.
aasAllocationId :: Lens' AllocateAddressResponse (Maybe Text)
