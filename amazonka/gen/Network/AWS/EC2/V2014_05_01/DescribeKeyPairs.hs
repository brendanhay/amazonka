{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.DescribeKeyPairs
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your key pairs. For more information about key
-- pairs, see Key Pairs in the Amazon Elastic Compute Cloud User Guide.
-- Example This example describes the keypair with name my-key-pair.
-- https://ec2.amazonaws.com/?Action=DescribeKeyPairs
-- &amp;KeyName.1=my-key-pair &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE my-key-pair
-- 1f:51:ae:28:bf:89:e9:d8:1f:25:5d:37:2d:7d:b8:ca:9f:f5:f1:6f Example This
-- example filters the response to include only key pairs whose names include
-- the string Dave. https://ec2.amazonaws.com/?Action=DescribeKeyPairs
-- &amp;Filter.1.Name=key-name &amp;Filter.1.Value.1=*Dave* &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_05_01.DescribeKeyPairs where

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

data DescribeKeyPairs = DescribeKeyPairs
    { _dkpsDryRun :: Maybe Bool
      -- ^ 
    , _dkpsFilters :: [Filter]
      -- ^ One or more filters. fingerprint - The fingerprint of the key
      -- pair. key-name - The name of the key pair.
    , _dkpsKeyNames :: [Text]
      -- ^ One or more key pair names. Default: Describes all your key
      -- pairs.
    } deriving (Generic)

instance ToQuery DescribeKeyPairs where
    toQuery = genericToQuery def

instance AWSRequest DescribeKeyPairs where
    type Sv DescribeKeyPairs = EC2
    type Rs DescribeKeyPairs = DescribeKeyPairsResponse

    request = post "DescribeKeyPairs"
    response _ = xmlResponse

data DescribeKeyPairsResponse = DescribeKeyPairsResponse
    { _dkptKeyPairs :: [KeyPairInfo]
      -- ^ Information about one or more key pairs.
    } deriving (Generic)

instance FromXML DescribeKeyPairsResponse where
    fromXMLOptions = xmlOptions
