{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

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

--
-- EC2 Requests
--

data EC2

instance AWSSigner EC2 where
    sign = version2

instance AWSRegion EC2 where
    regionalise reg rq = rq { rqHost = "ec2." <> toBS reg <> ".amazonaws.com" }

get :: QueryString a => ByteString -> a -> AWS (RawRequest EC2)
get = req GET

req :: QueryString a => Method -> ByteString -> a -> AWS (RawRequest EC2)
req meth action qry = return $ (emptyRequest meth version endpoint "" Nothing)
    { rqAction = Just action
    , rqQuery  = queryString qry
    }

version :: ApiVersion
version  = "2013-06-15"

endpoint :: ByteString
endpoint = "ec2.amazonaws.com"

--
-- Actions
--

-- |
--
--
data AllocateAddress = AllocateAddress
    { allocateAddressDomain :: !(Maybe ByteString)
    } deriving (Show)

$(deriveQS ''AllocateAddress)

instance AWSRequest EC2 AllocateAddress where
    request = get "AllocateAddress"

-- |
--
--
data DescribeInstances = DescribeInstances
    { describeInstancesInstanceId :: [ByteString]
    } deriving (Show)

$(deriveQS ''DescribeInstances)

instance AWSRequest EC2 DescribeInstances where
    request = get "DescribeInstances"

