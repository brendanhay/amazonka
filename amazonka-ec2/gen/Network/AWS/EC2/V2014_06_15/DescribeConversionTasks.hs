{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.EC2.V2014_06_15.DescribeConversionTasks where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeConversionTasks' request.
describeConversionTasks :: DescribeConversionTasks
describeConversionTasks = DescribeConversionTasks
    { _dctrDryRun = Nothing
    , _dctrConversionTaskIds = mempty
    , _dctrFilters = mempty
    }

data DescribeConversionTasks = DescribeConversionTasks
    { _dctrDryRun :: Maybe Bool
      -- ^ 
    , _dctrConversionTaskIds :: [Text]
      -- ^ One or more conversion task IDs.
    , _dctrFilters :: [Filter]
      -- ^ 
    } deriving (Show, Generic)

makeLenses ''DescribeConversionTasks

instance ToQuery DescribeConversionTasks where
    toQuery = genericQuery def

data DescribeConversionTasksResponse = DescribeConversionTasksResponse
    { _dctsConversionTasks :: [ConversionTask]
      -- ^ 
    } deriving (Show, Generic)

makeLenses ''DescribeConversionTasksResponse

instance FromXML DescribeConversionTasksResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeConversionTasks where
    type Sv DescribeConversionTasks = EC2
    type Rs DescribeConversionTasks = DescribeConversionTasksResponse

    request = post "DescribeConversionTasks"
    response _ = xmlResponse
