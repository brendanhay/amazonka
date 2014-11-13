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

-- Module      : Network.AWS.EC2.AllocateAddress
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
-- Addresses in the Amazon Elastic Compute Cloud User Guide.
module Network.AWS.EC2.AllocateAddress
    (
    -- * Request
      AllocateAddress
    -- ** Request constructor
    , allocateAddress
    -- ** Request lenses
    , aaDomain
    , aaDryRun

    -- * Response
    , AllocateAddressResponse
    -- ** Response constructor
    , allocateAddressResponse
    -- ** Response lenses
    , aarAllocationId
    , aarDomain
    , aarPublicIp
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data AllocateAddress = AllocateAddress
    { _aaDomain :: Maybe Text
    , _aaDryRun :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'AllocateAddress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aaDomain' @::@ 'Maybe' 'Text'
--
-- * 'aaDryRun' @::@ 'Maybe' 'Bool'
--
allocateAddress :: AllocateAddress
allocateAddress = AllocateAddress
    { _aaDryRun = Nothing
    , _aaDomain = Nothing
    }

-- | Set to vpc to allocate the address for use with instances in a VPC.
-- Default: The address is for use with instances in EC2-Classic.
aaDomain :: Lens' AllocateAddress (Maybe Text)
aaDomain = lens _aaDomain (\s a -> s { _aaDomain = a })

aaDryRun :: Lens' AllocateAddress (Maybe Bool)
aaDryRun = lens _aaDryRun (\s a -> s { _aaDryRun = a })

instance ToQuery AllocateAddress

instance ToPath AllocateAddress where
    toPath = const "/"

data AllocateAddressResponse = AllocateAddressResponse
    { _aarAllocationId :: Maybe Text
    , _aarDomain       :: Maybe Text
    , _aarPublicIp     :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AllocateAddressResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aarAllocationId' @::@ 'Maybe' 'Text'
--
-- * 'aarDomain' @::@ 'Maybe' 'Text'
--
-- * 'aarPublicIp' @::@ 'Maybe' 'Text'
--
allocateAddressResponse :: AllocateAddressResponse
allocateAddressResponse = AllocateAddressResponse
    { _aarPublicIp     = Nothing
    , _aarDomain       = Nothing
    , _aarAllocationId = Nothing
    }

-- | [EC2-VPC] The ID that AWS assigns to represent the allocation of the
-- Elastic IP address for use with instances in a VPC.
aarAllocationId :: Lens' AllocateAddressResponse (Maybe Text)
aarAllocationId = lens _aarAllocationId (\s a -> s { _aarAllocationId = a })

-- | Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic (standard) or instances in a VPC (vpc).
aarDomain :: Lens' AllocateAddressResponse (Maybe Text)
aarDomain = lens _aarDomain (\s a -> s { _aarDomain = a })

-- | The Elastic IP address.
aarPublicIp :: Lens' AllocateAddressResponse (Maybe Text)
aarPublicIp = lens _aarPublicIp (\s a -> s { _aarPublicIp = a })

instance AWSRequest AllocateAddress where
    type Sv AllocateAddress = EC2
    type Rs AllocateAddress = AllocateAddressResponse

    request  = post "AllocateAddress"
    response = xmlResponse $ \h x -> AllocateAddressResponse
        <$> x %| "allocationId"
        <*> x %| "domain"
        <*> x %| "publicIp"
