{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : Network.AWS.EC2
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.EC2 where

import Data.ByteString      (ByteString)
import Data.Monoid
import Network.AWS.Internal
import Network.Http.Client

ec2Sign :: QueryString a => Method -> ByteString -> Region -> a -> AWS SignedRequest
ec2Sign meth action reg qry = version2 rq
  where
    rq = (emptyRequest meth ec2Version (ec2Endpoint reg) "" Nothing)
        { rqAction = Just action
        , rqQuery  = queryString qry
        }

ec2Version :: ApiVersion
ec2Version  = "2013-06-15"

ec2Endpoint :: Region -> ByteString
ec2Endpoint reg = "ec2." <> toBS reg <> ".amazonaws.com"

data AllocateAddress = AllocateAddress
    { allocateAddressDomain :: !(Maybe ByteString)
    } deriving (Show)

$(deriveQS ''AllocateAddress)

instance RegionRequest AllocateAddress where
    signRegion = ec2Sign GET "AllocateAddress"

data DescribeInstances = DescribeInstances
    { describeInstancesInstanceId :: [ByteString]
    } deriving (Show)

$(deriveQS ''DescribeInstances)

instance RegionRequest DescribeInstances where
    signRegion = ec2Sign GET "DescribeInstances"

