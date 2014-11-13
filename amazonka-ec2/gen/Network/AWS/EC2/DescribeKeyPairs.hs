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

-- Module      : Network.AWS.EC2.DescribeKeyPairs
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
module Network.AWS.EC2.DescribeKeyPairs
    (
    -- * Request
      DescribeKeyPairs
    -- ** Request constructor
    , describeKeyPairs
    -- ** Request lenses
    , dkp1DryRun
    , dkp1Filters
    , dkp1KeyNames

    -- * Response
    , DescribeKeyPairsResponse
    -- ** Response constructor
    , describeKeyPairsResponse
    -- ** Response lenses
    , dkprKeyPairs
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DescribeKeyPairs = DescribeKeyPairs
    { _dkp1DryRun   :: Maybe Bool
    , _dkp1Filters  :: [Filter]
    , _dkp1KeyNames :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'DescribeKeyPairs' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dkp1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dkp1Filters' @::@ ['Filter']
--
-- * 'dkp1KeyNames' @::@ ['Text']
--
describeKeyPairs :: DescribeKeyPairs
describeKeyPairs = DescribeKeyPairs
    { _dkp1DryRun   = Nothing
    , _dkp1KeyNames = mempty
    , _dkp1Filters  = mempty
    }

dkp1DryRun :: Lens' DescribeKeyPairs (Maybe Bool)
dkp1DryRun = lens _dkp1DryRun (\s a -> s { _dkp1DryRun = a })

-- | One or more filters. fingerprint - The fingerprint of the key pair.
-- key-name - The name of the key pair.
dkp1Filters :: Lens' DescribeKeyPairs [Filter]
dkp1Filters = lens _dkp1Filters (\s a -> s { _dkp1Filters = a })

-- | One or more key pair names. Default: Describes all your key pairs.
dkp1KeyNames :: Lens' DescribeKeyPairs [Text]
dkp1KeyNames = lens _dkp1KeyNames (\s a -> s { _dkp1KeyNames = a })

instance ToQuery DescribeKeyPairs

instance ToPath DescribeKeyPairs where
    toPath = const "/"

newtype DescribeKeyPairsResponse = DescribeKeyPairsResponse
    { _dkprKeyPairs :: [KeyPairInfo]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList DescribeKeyPairsResponse where
    type Item DescribeKeyPairsResponse = KeyPairInfo

    fromList = DescribeKeyPairsResponse . fromList
    toList   = toList . _dkprKeyPairs

-- | 'DescribeKeyPairsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dkprKeyPairs' @::@ ['KeyPairInfo']
--
describeKeyPairsResponse :: DescribeKeyPairsResponse
describeKeyPairsResponse = DescribeKeyPairsResponse
    { _dkprKeyPairs = mempty
    }

-- | Information about one or more key pairs.
dkprKeyPairs :: Lens' DescribeKeyPairsResponse [KeyPairInfo]
dkprKeyPairs = lens _dkprKeyPairs (\s a -> s { _dkprKeyPairs = a })

instance FromXML DescribeKeyPairsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeKeyPairsResponse"

instance AWSRequest DescribeKeyPairs where
    type Sv DescribeKeyPairs = EC2
    type Rs DescribeKeyPairs = DescribeKeyPairsResponse

    request  = post "DescribeKeyPairs"
    response = xmlResponse $ \h x -> DescribeKeyPairsResponse
        <$> x %| "keySet"
