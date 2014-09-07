{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.V2009_03_31.DescribeStep
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides more detail about the cluster step.
module Network.AWS.EMR.V2009_03_31.DescribeStep
    (
    -- * Request
      DescribeStep
    -- ** Request constructor
    , mkDescribeStep
    -- ** Request lenses
    , dsClusterId
    , dsStepId

    -- * Response
    , DescribeStepResponse
    -- ** Response lenses
    , dsrsStep
    ) where

import Network.AWS.EMR.V2009_03_31.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | This input determines which step to describe.
data DescribeStep = DescribeStep
    { _dsClusterId :: Text
    , _dsStepId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeStep' request.
mkDescribeStep :: Text -- ^ 'dsClusterId'
               -> Text -- ^ 'dsStepId'
               -> DescribeStep
mkDescribeStep p1 p2 = DescribeStep
    { _dsClusterId = p1
    , _dsStepId = p2
    }

-- | The identifier of the cluster with steps to describe.
dsClusterId :: Lens' DescribeStep Text
dsClusterId = lens _dsClusterId (\s a -> s { _dsClusterId = a })

-- | The identifier of the step to describe.
dsStepId :: Lens' DescribeStep Text
dsStepId = lens _dsStepId (\s a -> s { _dsStepId = a })

instance ToPath DescribeStep

instance ToQuery DescribeStep

instance ToHeaders DescribeStep

instance ToJSON DescribeStep

-- | This output contains the description of the cluster step.
newtype DescribeStepResponse = DescribeStepResponse
    { _dsrsStep :: Maybe Step
    } deriving (Show, Generic)

-- | The step details for the requested step identifier.
dsrsStep :: Lens' DescribeStepResponse (Maybe Step)
dsrsStep = lens _dsrsStep (\s a -> s { _dsrsStep = a })

instance FromJSON DescribeStepResponse

instance AWSRequest DescribeStep where
    type Sv DescribeStep = EMR
    type Rs DescribeStep = DescribeStepResponse

    request = get
    response _ = jsonResponse
