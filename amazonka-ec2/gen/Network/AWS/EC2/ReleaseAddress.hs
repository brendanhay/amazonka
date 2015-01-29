{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ReleaseAddress
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Releases the specified Elastic IP address.
--
-- After releasing an Elastic IP address, it is released to the IP address pool
-- and might be unavailable to you. Be sure to update your DNS records and any
-- servers or devices that communicate with the address. If you attempt to
-- release an Elastic IP address that you already released, you'll get an 'AuthFailure' error if the address is already allocated to another AWS account.
--
-- [EC2-Classic, default VPC] Releasing an Elastic IP address automatically
-- disassociates it from any instance that it's associated with. To disassociate
-- an Elastic IP address without releasing it, use 'DisassociateAddress'.
--
-- [Nondefault VPC] You must use 'DisassociateAddress' to disassociate the
-- Elastic IP address before you try to release it. Otherwise, Amazon EC2
-- returns an error ('InvalidIPAddress.InUse').
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReleaseAddress.html>
module Network.AWS.EC2.ReleaseAddress
    (
    -- * Request
      ReleaseAddress
    -- ** Request constructor
    , releaseAddress
    -- ** Request lenses
    , raAllocationId
    , raDryRun
    , raPublicIp

    -- * Response
    , ReleaseAddressResponse
    -- ** Response constructor
    , releaseAddressResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ReleaseAddress = ReleaseAddress
    { _raAllocationId :: Maybe Text
    , _raDryRun       :: Maybe Bool
    , _raPublicIp     :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ReleaseAddress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'raAllocationId' @::@ 'Maybe' 'Text'
--
-- * 'raDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'raPublicIp' @::@ 'Maybe' 'Text'
--
releaseAddress :: ReleaseAddress
releaseAddress = ReleaseAddress
    { _raDryRun       = Nothing
    , _raPublicIp     = Nothing
    , _raAllocationId = Nothing
    }

-- | [EC2-VPC] The allocation ID. Required for EC2-VPC.
raAllocationId :: Lens' ReleaseAddress (Maybe Text)
raAllocationId = lens _raAllocationId (\s a -> s { _raAllocationId = a })

raDryRun :: Lens' ReleaseAddress (Maybe Bool)
raDryRun = lens _raDryRun (\s a -> s { _raDryRun = a })

-- | [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
raPublicIp :: Lens' ReleaseAddress (Maybe Text)
raPublicIp = lens _raPublicIp (\s a -> s { _raPublicIp = a })

data ReleaseAddressResponse = ReleaseAddressResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'ReleaseAddressResponse' constructor.
releaseAddressResponse :: ReleaseAddressResponse
releaseAddressResponse = ReleaseAddressResponse

instance ToPath ReleaseAddress where
    toPath = const "/"

instance ToQuery ReleaseAddress where
    toQuery ReleaseAddress{..} = mconcat
        [ "AllocationId" =? _raAllocationId
        , "DryRun"       =? _raDryRun
        , "PublicIp"     =? _raPublicIp
        ]

instance ToHeaders ReleaseAddress

instance AWSRequest ReleaseAddress where
    type Sv ReleaseAddress = EC2
    type Rs ReleaseAddress = ReleaseAddressResponse

    request  = post "ReleaseAddress"
    response = nullResponse ReleaseAddressResponse
