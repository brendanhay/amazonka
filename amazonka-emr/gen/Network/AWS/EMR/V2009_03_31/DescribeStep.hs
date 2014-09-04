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
    , mkDescribeStepInput
    -- ** Request lenses
    , dsiClusterId
    , dsiStepId

    -- * Response
    , DescribeStepResponse
    -- ** Response lenses
    , dsoStep
    ) where

import           Network.AWS.EMR.V2009_03_31.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeStep' request.
mkDescribeStepInput :: Text -- ^ 'dsiClusterId'
                    -> Text -- ^ 'dsiStepId'
                    -> DescribeStep
mkDescribeStepInput p1 p2 = DescribeStep
    { _dsiClusterId = p1
    , _dsiStepId = p2
    }
{-# INLINE mkDescribeStepInput #-}

data DescribeStep = DescribeStep
    { _dsiClusterId :: Text
      -- ^ The identifier of the cluster with steps to describe.
    , _dsiStepId :: Text
      -- ^ The identifier of the step to describe.
    } deriving (Show, Generic)

-- | The identifier of the cluster with steps to describe.
dsiClusterId :: Lens' DescribeStep (Text)
dsiClusterId = lens _dsiClusterId (\s a -> s { _dsiClusterId = a })
{-# INLINE dsiClusterId #-}

-- | The identifier of the step to describe.
dsiStepId :: Lens' DescribeStep (Text)
dsiStepId = lens _dsiStepId (\s a -> s { _dsiStepId = a })
{-# INLINE dsiStepId #-}

instance ToPath DescribeStep

instance ToQuery DescribeStep

instance ToHeaders DescribeStep

instance ToJSON DescribeStep

newtype DescribeStepResponse = DescribeStepResponse
    { _dsoStep :: Maybe Step
      -- ^ The step details for the requested step identifier.
    } deriving (Show, Generic)

-- | The step details for the requested step identifier.
dsoStep :: Lens' DescribeStepResponse (Maybe Step)
dsoStep = lens _dsoStep (\s a -> s { _dsoStep = a })
{-# INLINE dsoStep #-}

instance FromJSON DescribeStepResponse

instance AWSRequest DescribeStep where
    type Sv DescribeStep = EMR
    type Rs DescribeStep = DescribeStepResponse

    request = get
    response _ = jsonResponse
