{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeKeyPairs
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
module Network.AWS.EC2.V2014_06_15.DescribeKeyPairs
    (
    -- * Request
      DescribeKeyPairs
    -- ** Default constructor
    , describeKeyPairs
    -- ** Accessors and lenses
    , _dkpsFilters
    , dkpsFilters
    , _dkpsKeyNames
    , dkpsKeyNames

    -- * Response
    , DescribeKeyPairsResponse
    -- ** Accessors and lenses
    , _dkptKeyPairs
    , dkptKeyPairs
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeKeyPairs' request.
describeKeyPairs :: DescribeKeyPairs
describeKeyPairs = DescribeKeyPairs
    { _dkpsFilters = mempty
    , _dkpsKeyNames = mempty
    }

data DescribeKeyPairs = DescribeKeyPairs

makeSiglessLenses ''DescribeKeyPairs

instance ToQuery DescribeKeyPairs where
    toQuery = genericQuery def

data DescribeKeyPairsResponse = DescribeKeyPairsResponse
    { _dkptKeyPairs :: [KeyPairInfo]
      -- ^ Information about one or more key pairs.
    } deriving (Show, Generic)

makeSiglessLenses ''DescribeKeyPairsResponse

instance FromXML DescribeKeyPairsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeKeyPairs where
    type Sv DescribeKeyPairs = EC2
    type Rs DescribeKeyPairs = DescribeKeyPairsResponse

    request = post "DescribeKeyPairs"
    response _ = xmlResponse

-- | One or more filters. fingerprint - The fingerprint of the key pair.
-- key-name - The name of the key pair.
dkpsFilters :: Lens' DescribeKeyPairs ([Filter])

-- | One or more key pair names. Default: Describes all your key pairs.
dkpsKeyNames :: Lens' DescribeKeyPairs ([Text])

-- | Information about one or more key pairs.
dkptKeyPairs :: Lens' DescribeKeyPairsResponse ([KeyPairInfo])
