{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeConversionTasks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your conversion tasks. For more information, see
-- Using the Command Line Tools to Import Your Virtual Machine to Amazon EC2
-- in the Amazon Elastic Compute Cloud User Guide. Example This example
-- describes all your conversion tasks.
-- https://ec2.amazonaws.com/?Action=DescribeConversionTasks &amp;AUTHPARAMS
-- import-i-fh95npoc 2010-12-22T12:01Z 1000 us-east-1a VDMK 128696320
-- https://s3.amazonaws.com/myawsbucket/​a3a5e1b6-590d-43cc-97c1-15c7325d3f41/​Win_2008_Server_Data_Center_SP2_32-bit.​vmdkmanifest.xml?AWSAccessKeyId=​AKIAIOSFODNN7EXAMPLE&amp;​Expires=1294855591&amp;​Signature=5snej01TlTtL0uR7KExtEXAMPLE%3D
-- 8 vol-34d8a2ff active.
module Network.AWS.EC2.V2014_06_15.DescribeConversionTasks
    (
    -- * Request
      DescribeConversionTasks
    -- ** Request constructor
    , mkDescribeConversionTasks
    -- ** Request lenses
    , dctFilters
    , dctConversionTaskIds

    -- * Response
    , DescribeConversionTasksResponse
    -- ** Response lenses
    , dctrsConversionTasks
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data DescribeConversionTasks = DescribeConversionTasks
    { _dctFilters :: [Filter]
    , _dctConversionTaskIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeConversionTasks' request.
mkDescribeConversionTasks :: DescribeConversionTasks
mkDescribeConversionTasks = DescribeConversionTasks
    { _dctFilters = mempty
    , _dctConversionTaskIds = mempty
    }

-- | 
dctFilters :: Lens' DescribeConversionTasks [Filter]
dctFilters = lens _dctFilters (\s a -> s { _dctFilters = a })

-- | One or more conversion task IDs.
dctConversionTaskIds :: Lens' DescribeConversionTasks [Text]
dctConversionTaskIds =
    lens _dctConversionTaskIds (\s a -> s { _dctConversionTaskIds = a })

instance ToQuery DescribeConversionTasks where
    toQuery = genericQuery def

-- | 
newtype DescribeConversionTasksResponse = DescribeConversionTasksResponse
    { _dctrsConversionTasks :: [ConversionTask]
    } deriving (Show, Generic)

-- | 
dctrsConversionTasks :: Lens' DescribeConversionTasksResponse [ConversionTask]
dctrsConversionTasks =
    lens _dctrsConversionTasks (\s a -> s { _dctrsConversionTasks = a })

instance FromXML DescribeConversionTasksResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeConversionTasks where
    type Sv DescribeConversionTasks = EC2
    type Rs DescribeConversionTasks = DescribeConversionTasksResponse

    request = post "DescribeConversionTasks"
    response _ = xmlResponse
