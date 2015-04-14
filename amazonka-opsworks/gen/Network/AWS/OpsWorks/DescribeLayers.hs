{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Requests a description of one or more layers in a specified stack.
--
-- You must specify at least one of the parameters.
--
-- Required Permissions: To use this action, an IAM user must have a Show,
-- Deploy, or Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeLayers.html>
module Network.AWS.OpsWorks.DescribeLayers
    (
    -- * Request
      DescribeLayers
    -- ** Request constructor
    , describeLayers
    -- ** Request lenses
    , dlLayerIds
    , dlStackId

    -- * Response
    , DescribeLayersResponse
    -- ** Response constructor
    , describeLayersResponse
    -- ** Response lenses
    , dlrLayers
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data DescribeLayers = DescribeLayers
    { _dlLayerIds :: List "LayerIds" Text
    , _dlStackId  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeLayers' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlLayerIds' @::@ ['Text']
--
-- * 'dlStackId' @::@ 'Maybe' 'Text'
--
describeLayers :: DescribeLayers
describeLayers = DescribeLayers
    { _dlStackId  = Nothing
    , _dlLayerIds = mempty
    }

-- | An array of layer IDs that specify the layers to be described. If you omit
-- this parameter, 'DescribeLayers' returns a description of every layer in the
-- specified stack.
dlLayerIds :: Lens' DescribeLayers [Text]
dlLayerIds = lens _dlLayerIds (\s a -> s { _dlLayerIds = a }) . _List

-- | The stack ID.
dlStackId :: Lens' DescribeLayers (Maybe Text)
dlStackId = lens _dlStackId (\s a -> s { _dlStackId = a })

newtype DescribeLayersResponse = DescribeLayersResponse
    { _dlrLayers :: List "Layers" Layer
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeLayersResponse where
    type Item DescribeLayersResponse = Layer

    fromList = DescribeLayersResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dlrLayers

-- | 'DescribeLayersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlrLayers' @::@ ['Layer']
--
describeLayersResponse :: DescribeLayersResponse
describeLayersResponse = DescribeLayersResponse
    { _dlrLayers = mempty
    }

-- | An array of 'Layer' objects that describe the layers.
dlrLayers :: Lens' DescribeLayersResponse [Layer]
dlrLayers = lens _dlrLayers (\s a -> s { _dlrLayers = a }) . _List

instance ToPath DescribeLayers where
    toPath = const "/"

instance ToQuery DescribeLayers where
    toQuery = const mempty

instance ToHeaders DescribeLayers

instance ToJSON DescribeLayers where
    toJSON DescribeLayers{..} = object
        [ "StackId"  .= _dlStackId
        , "LayerIds" .= _dlLayerIds
        ]

instance AWSRequest DescribeLayers where
    type Sv DescribeLayers = OpsWorks
    type Rs DescribeLayers = DescribeLayersResponse

    request  = post "DescribeLayers"
    response = jsonResponse

instance FromJSON DescribeLayersResponse where
    parseJSON = withObject "DescribeLayersResponse" $ \o -> DescribeLayersResponse
        <$> o .:? "Layers" .!= mempty
