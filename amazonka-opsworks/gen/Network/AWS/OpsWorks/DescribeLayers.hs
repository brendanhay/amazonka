{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribeLayers
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
module Network.AWS.OpsWorks
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
    -- ** Response constructor
    , mkDescribeLayersResponse
    -- ** Response lenses
    , dlrLayers
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DescribeLayers = DescribeLayers
    { _dl1StackId :: !(Maybe Text)
    , _dl1LayerIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLayers' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Maybe Text@
--
-- * @LayerIds ::@ @[Text]@
--
mkDescribeLayers :: DescribeLayers
mkDescribeLayers = DescribeLayers
    { _dl1StackId = Nothing
    , _dl1LayerIds = mempty
    }

-- | The stack ID.
dl1StackId :: Lens' DescribeLayers (Maybe Text)
dl1StackId = lens _dl1StackId (\s a -> s { _dl1StackId = a })

-- | An array of layer IDs that specify the layers to be described. If you omit
-- this parameter, DescribeLayers returns a description of every layer in the
-- specified stack.
dl1LayerIds :: Lens' DescribeLayers [Text]
dl1LayerIds = lens _dl1LayerIds (\s a -> s { _dl1LayerIds = a })

instance ToPath DescribeLayers

instance ToQuery DescribeLayers

instance ToHeaders DescribeLayers

instance ToJSON DescribeLayers

-- | Contains the response to a DescribeLayers request.
newtype DescribeLayersResponse = DescribeLayersResponse
    { _dlrLayers :: [Layer]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLayersResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Layers ::@ @[Layer]@
--
mkDescribeLayersResponse :: DescribeLayersResponse
mkDescribeLayersResponse = DescribeLayersResponse
    { _dlrLayers = mempty
    }

-- | An array of Layer objects that describe the layers.
dlrLayers :: Lens' DescribeLayersResponse [Layer]
dlrLayers = lens _dlrLayers (\s a -> s { _dlrLayers = a })

instance FromJSON DescribeLayersResponse

instance AWSRequest DescribeLayers where
    type Sv DescribeLayers = OpsWorks
    type Rs DescribeLayers = DescribeLayersResponse

    request = get
    response _ = jsonResponse
