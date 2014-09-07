{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribeRdsDbInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes Amazon RDS instances.
module Network.AWS.OpsWorks.V2013_02_18.DescribeRdsDbInstances
    (
    -- * Request
      DescribeRdsDbInstances
    -- ** Request constructor
    , mkDescribeRdsDbInstances
    -- ** Request lenses
    , drdi1StackId
    , drdi1RdsDbInstanceArns

    -- * Response
    , DescribeRdsDbInstancesResponse
    -- ** Response lenses
    , drdirsRdsDbInstances
    ) where

import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DescribeRdsDbInstances = DescribeRdsDbInstances
    { _drdi1StackId :: Text
    , _drdi1RdsDbInstanceArns :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeRdsDbInstances' request.
mkDescribeRdsDbInstances :: Text -- ^ 'drdi1StackId'
                         -> DescribeRdsDbInstances
mkDescribeRdsDbInstances p1 = DescribeRdsDbInstances
    { _drdi1StackId = p1
    , _drdi1RdsDbInstanceArns = mempty
    }

-- | The stack ID that the instances are registered with. The operation returns
-- descriptions of all registered Amazon RDS instances.
drdi1StackId :: Lens' DescribeRdsDbInstances Text
drdi1StackId = lens _drdi1StackId (\s a -> s { _drdi1StackId = a })

-- | An array containing the ARNs of the instances to be described.
drdi1RdsDbInstanceArns :: Lens' DescribeRdsDbInstances [Text]
drdi1RdsDbInstanceArns =
    lens _drdi1RdsDbInstanceArns (\s a -> s { _drdi1RdsDbInstanceArns = a })

instance ToPath DescribeRdsDbInstances

instance ToQuery DescribeRdsDbInstances

instance ToHeaders DescribeRdsDbInstances

instance ToJSON DescribeRdsDbInstances

-- | Contains the response to a DescribeRdsDbInstances request.
newtype DescribeRdsDbInstancesResponse = DescribeRdsDbInstancesResponse
    { _drdirsRdsDbInstances :: [RdsDbInstance]
    } deriving (Show, Generic)

-- | An a array of RdsDbInstance objects that describe the instances.
drdirsRdsDbInstances :: Lens' DescribeRdsDbInstancesResponse [RdsDbInstance]
drdirsRdsDbInstances =
    lens _drdirsRdsDbInstances (\s a -> s { _drdirsRdsDbInstances = a })

instance FromJSON DescribeRdsDbInstancesResponse

instance AWSRequest DescribeRdsDbInstances where
    type Sv DescribeRdsDbInstances = OpsWorks
    type Rs DescribeRdsDbInstances = DescribeRdsDbInstancesResponse

    request = get
    response _ = jsonResponse
