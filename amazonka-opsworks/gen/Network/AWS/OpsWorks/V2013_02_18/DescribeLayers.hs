{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.OpsWorks.V2013_02_18.DescribeLayers
    (
    -- * Request
      DescribeLayers
    -- ** Request constructor
    , mkDescribeLayers
    -- ** Request lenses
    , dl1StackId
    , dl1LayerIds

    -- * Response
    , DescribeLayersResponse
    -- ** Response lenses
    , dlrsLayers
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data DescribeLayers = DescribeLayers
    { _dl1StackId :: Maybe Text
    , _dl1LayerIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLayers' request.
mkDescribeLayers :: DescribeLayers
mkDescribeLayers = DescribeLayers
    { _dl1StackId = Nothing
    , _dl1LayerIds = mempty
    }
{-# INLINE mkDescribeLayers #-}

-- | The stack ID.
dl1StackId :: Lens' DescribeLayers (Maybe Text)
dl1StackId = lens _dl1StackId (\s a -> s { _dl1StackId = a })
{-# INLINE dl1StackId #-}

-- | An array of layer IDs that specify the layers to be described. If you omit
-- this parameter, DescribeLayers returns a description of every layer in the
-- specified stack.
dl1LayerIds :: Lens' DescribeLayers [Text]
dl1LayerIds = lens _dl1LayerIds (\s a -> s { _dl1LayerIds = a })
{-# INLINE dl1LayerIds #-}

instance ToPath DescribeLayers

instance ToQuery DescribeLayers

instance ToHeaders DescribeLayers

instance ToJSON DescribeLayers

-- | Contains the response to a DescribeLayers request.
newtype DescribeLayersResponse = DescribeLayersResponse
    { _dlrsLayers :: [Layer]
    } deriving (Show, Generic)

-- | An array of Layer objects that describe the layers.
dlrsLayers :: Lens' DescribeLayersResponse [Layer]
dlrsLayers = lens _dlrsLayers (\s a -> s { _dlrsLayers = a })
{-# INLINE dlrsLayers #-}

instance FromJSON DescribeLayersResponse

instance AWSRequest DescribeLayers where
    type Sv DescribeLayers = OpsWorks
    type Rs DescribeLayers = DescribeLayersResponse

    request = get
    response _ = jsonResponse
