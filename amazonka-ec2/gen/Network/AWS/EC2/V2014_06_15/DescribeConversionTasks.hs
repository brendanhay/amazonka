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
    , mkDescribeConversionTasksRequest
    -- ** Request lenses
    , dctrFilters
    , dctrConversionTaskIds

    -- * Response
    , DescribeConversionTasksResponse
    -- ** Response lenses
    , dctsConversionTasks
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeConversionTasks' request.
mkDescribeConversionTasksRequest :: DescribeConversionTasks
mkDescribeConversionTasksRequest = DescribeConversionTasks
    { _dctrFilters = mempty
    , _dctrConversionTaskIds = mempty
    }
{-# INLINE mkDescribeConversionTasksRequest #-}

data DescribeConversionTasks = DescribeConversionTasks
    { _dctrFilters :: [Filter]
      -- ^ 
    , _dctrConversionTaskIds :: [Text]
      -- ^ One or more conversion task IDs.
    } deriving (Show, Generic)

-- | 
dctrFilters :: Lens' DescribeConversionTasks ([Filter])
dctrFilters = lens _dctrFilters (\s a -> s { _dctrFilters = a })
{-# INLINE dctrFilters #-}

-- | One or more conversion task IDs.
dctrConversionTaskIds :: Lens' DescribeConversionTasks ([Text])
dctrConversionTaskIds = lens _dctrConversionTaskIds (\s a -> s { _dctrConversionTaskIds = a })
{-# INLINE dctrConversionTaskIds #-}

instance ToQuery DescribeConversionTasks where
    toQuery = genericQuery def

newtype DescribeConversionTasksResponse = DescribeConversionTasksResponse
    { _dctsConversionTasks :: [ConversionTask]
      -- ^ 
    } deriving (Show, Generic)

-- | 
dctsConversionTasks :: Lens' DescribeConversionTasksResponse ([ConversionTask])
dctsConversionTasks = lens _dctsConversionTasks (\s a -> s { _dctsConversionTasks = a })
{-# INLINE dctsConversionTasks #-}

instance FromXML DescribeConversionTasksResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeConversionTasks where
    type Sv DescribeConversionTasks = EC2
    type Rs DescribeConversionTasks = DescribeConversionTasksResponse

    request = post "DescribeConversionTasks"
    response _ = xmlResponse
