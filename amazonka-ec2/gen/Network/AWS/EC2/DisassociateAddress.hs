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

-- Module      : Network.AWS.EC2.DisassociateAddress
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

-- | Disassociates an Elastic IP address from the instance or network interface
-- it's associated with.
--
-- An Elastic IP address is for use in either the EC2-Classic platform or in a
-- VPC. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses> in the /Amazon ElasticCompute Cloud User Guide for Linux/.
--
-- This is an idempotent operation. If you perform the operation more than
-- once, Amazon EC2 doesn't return an error.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DisassociateAddress.html>
module Network.AWS.EC2.DisassociateAddress
    (
    -- * Request
      DisassociateAddress
    -- ** Request constructor
    , disassociateAddress
    -- ** Request lenses
    , da1AssociationId
    , da1DryRun
    , da1PublicIp

    -- * Response
    , DisassociateAddressResponse
    -- ** Response constructor
    , disassociateAddressResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DisassociateAddress = DisassociateAddress
    { _da1AssociationId :: Maybe Text
    , _da1DryRun        :: Maybe Bool
    , _da1PublicIp      :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DisassociateAddress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'da1AssociationId' @::@ 'Maybe' 'Text'
--
-- * 'da1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'da1PublicIp' @::@ 'Maybe' 'Text'
--
disassociateAddress :: DisassociateAddress
disassociateAddress = DisassociateAddress
    { _da1DryRun        = Nothing
    , _da1PublicIp      = Nothing
    , _da1AssociationId = Nothing
    }

-- | [EC2-VPC] The association ID. Required for EC2-VPC.
da1AssociationId :: Lens' DisassociateAddress (Maybe Text)
da1AssociationId = lens _da1AssociationId (\s a -> s { _da1AssociationId = a })

da1DryRun :: Lens' DisassociateAddress (Maybe Bool)
da1DryRun = lens _da1DryRun (\s a -> s { _da1DryRun = a })

-- | [EC2-Classic] The Elastic IP address. Required for EC2-Classic.
da1PublicIp :: Lens' DisassociateAddress (Maybe Text)
da1PublicIp = lens _da1PublicIp (\s a -> s { _da1PublicIp = a })

data DisassociateAddressResponse = DisassociateAddressResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DisassociateAddressResponse' constructor.
disassociateAddressResponse :: DisassociateAddressResponse
disassociateAddressResponse = DisassociateAddressResponse

instance ToPath DisassociateAddress where
    toPath = const "/"

instance ToQuery DisassociateAddress where
    toQuery DisassociateAddress{..} = mconcat
        [ "AssociationId" =? _da1AssociationId
        , "DryRun"        =? _da1DryRun
        , "PublicIp"      =? _da1PublicIp
        ]

instance ToHeaders DisassociateAddress

instance AWSRequest DisassociateAddress where
    type Sv DisassociateAddress = EC2
    type Rs DisassociateAddress = DisassociateAddressResponse

    request  = post "DisassociateAddress"
    response = nullResponse DisassociateAddressResponse
