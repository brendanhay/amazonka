{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribeLayers
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests a description of one or more layers in a specified stack. You must
-- specify at least one of the parameters. Required Permissions: To use this
-- action, an IAM user must have a Show, Deploy, or Manage permissions level
-- for the stack, or an attached policy that explicitly grants permissions.
-- For more information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DescribeLayers where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeLayers' request.
describeLayers :: DescribeLayers
describeLayers = DescribeLayers
    { _dlrStackId = Nothing
    , _dlrLayerIds = mempty
    }

data DescribeLayers = DescribeLayers
    { _dlrStackId :: Maybe Text
      -- ^ The stack ID.
    , _dlrLayerIds :: [Text]
      -- ^ An array of layer IDs that specify the layers to be described. If
      -- you omit this parameter, DescribeLayers returns a description of
      -- every layer in the specified stack.
    } deriving (Generic)

makeLenses ''DescribeLayers

instance ToPath DescribeLayers

instance ToQuery DescribeLayers

instance ToHeaders DescribeLayers

instance ToJSON DescribeLayers

data DescribeLayersResponse = DescribeLayersResponse
    { _dlsLayers :: [Layer]
      -- ^ An array of Layer objects that describe the layers.
    } deriving (Generic)

makeLenses ''DescribeLayersResponse

instance FromJSON DescribeLayersResponse

instance AWSRequest DescribeLayers where
    type Sv DescribeLayers = OpsWorks
    type Rs DescribeLayers = DescribeLayersResponse

    request = get
    response _ = jsonResponse
