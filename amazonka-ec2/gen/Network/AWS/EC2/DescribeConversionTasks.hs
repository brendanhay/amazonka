{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeConversionTasks
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
module Network.AWS.EC2.DescribeConversionTasks
    (
    -- * Request
      DescribeConversionTasks
    -- ** Request constructor
    , describeConversionTasks
    -- ** Request lenses
    , dctFilter
    , dctConversionTaskId

    -- * Response
    , DescribeConversionTasksResponse
    -- ** Response constructor
    , describeConversionTasksResponse
    -- ** Response lenses
    , dctrItem
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DescribeConversionTasks = DescribeConversionTasks
    { _dctFilter :: [Filter]
    , _dctConversionTaskId :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeConversionTasks' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Filter ::@ @[Filter]@
--
-- * @ConversionTaskId ::@ @[Text]@
--
describeConversionTasks :: DescribeConversionTasks
describeConversionTasks = DescribeConversionTasks
    { _dctFilter = mempty
    , _dctConversionTaskId = mempty
    }

-- | 
dctFilter :: Lens' DescribeConversionTasks [Filter]
dctFilter = lens _dctFilter (\s a -> s { _dctFilter = a })

-- | One or more conversion task IDs.
dctConversionTaskId :: Lens' DescribeConversionTasks [Text]
dctConversionTaskId =
    lens _dctConversionTaskId (\s a -> s { _dctConversionTaskId = a })

instance ToQuery DescribeConversionTasks where
    toQuery = genericQuery def

newtype DescribeConversionTasksResponse = DescribeConversionTasksResponse
    { _dctrItem :: [ConversionTask]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeConversionTasksResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Item ::@ @[ConversionTask]@
--
describeConversionTasksResponse :: DescribeConversionTasksResponse
describeConversionTasksResponse = DescribeConversionTasksResponse
    { _dctrItem = mempty
    }

-- | 
dctrItem :: Lens' DescribeConversionTasksResponse [ConversionTask]
dctrItem = lens _dctrItem (\s a -> s { _dctrItem = a })

instance FromXML DescribeConversionTasksResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeConversionTasks where
    type Sv DescribeConversionTasks = EC2
    type Rs DescribeConversionTasks = DescribeConversionTasksResponse

    request = post "DescribeConversionTasks"
    response _ = xmlResponse
