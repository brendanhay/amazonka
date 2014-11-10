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
    , dkpDryRun
    , dkpFilters
    , dkpKeyNames

    -- * Response
    , DescribeKeyPairsResult
    -- ** Response constructor
    , describeKeyPairsResponse
    -- ** Response lenses
    , dkprKeyPairs
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DescribeKeyPairs = DescribeKeyPairs
    { _dkpDryRun   :: Maybe Bool
    , _dkpFilters  :: [Filter]
    , _dkpKeyNames :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'DescribeKeyPairs' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dkpDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dkpFilters' @::@ ['Filter']
--
-- * 'dkpKeyNames' @::@ ['Text']
--
describeKeyPairs :: DescribeKeyPairs
describeKeyPairs = DescribeKeyPairs
    { _dkpDryRun   = Nothing
    , _dkpKeyNames = mempty
    , _dkpFilters  = mempty
    }

dkpDryRun :: Lens' DescribeKeyPairs (Maybe Bool)
dkpDryRun = lens _dkpDryRun (\s a -> s { _dkpDryRun = a })

-- | One or more filters. fingerprint - The fingerprint of the key pair.
-- key-name - The name of the key pair.
dkpFilters :: Lens' DescribeKeyPairs [Filter]
dkpFilters = lens _dkpFilters (\s a -> s { _dkpFilters = a })

-- | One or more key pair names. Default: Describes all your key pairs.
dkpKeyNames :: Lens' DescribeKeyPairs [Text]
dkpKeyNames = lens _dkpKeyNames (\s a -> s { _dkpKeyNames = a })

instance ToPath DescribeKeyPairs where
    toPath = const "/"

instance ToQuery DescribeKeyPairs

newtype DescribeKeyPairsResult = DescribeKeyPairsResult
    { _dkprKeyPairs :: [KeyPairInfo]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'DescribeKeyPairsResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dkprKeyPairs' @::@ ['KeyPairInfo']
--
describeKeyPairsResponse :: DescribeKeyPairsResult
describeKeyPairsResponse = DescribeKeyPairsResult
    { _dkprKeyPairs = mempty
    }

-- | Information about one or more key pairs.
dkprKeyPairs :: Lens' DescribeKeyPairsResult [KeyPairInfo]
dkprKeyPairs = lens _dkprKeyPairs (\s a -> s { _dkprKeyPairs = a })

instance AWSRequest DescribeKeyPairs where
    type Sv DescribeKeyPairs = EC2
    type Rs DescribeKeyPairs = DescribeKeyPairsResult

    request  = post "DescribeKeyPairs"
    response = xmlResponse $ \h x -> DescribeKeyPairsResult
        <$> x %| "keySet"
