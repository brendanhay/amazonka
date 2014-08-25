{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.EMR.V2009_03_31.DescribeStep where

import           Network.AWS.EMR.V2009_03_31.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data DescribeStep = DescribeStep
    { _dsiClusterId :: Text
      -- ^ The identifier of the cluster with steps to describe.
    , _dsiStepId :: Text
      -- ^ The identifier of the step to describe.
    } deriving (Show, Generic)

makeLenses ''DescribeStep

instance ToPath DescribeStep

instance ToQuery DescribeStep

instance ToHeaders DescribeStep

instance ToJSON DescribeStep

data DescribeStepResponse = DescribeStepResponse
    { _dsoStep :: Maybe Step
      -- ^ The step details for the requested step identifier.
    } deriving (Show, Generic)

makeLenses ''DescribeStepResponse

instance FromJSON DescribeStepResponse

instance AWSRequest DescribeStep where
    type Sv DescribeStep = EMR
    type Rs DescribeStep = DescribeStepResponse

    request = get
    response _ = jsonResponse
