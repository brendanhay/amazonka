{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.DescribeLayers
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
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

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLayers' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlLayerIds'
--
-- * 'dlStackId'
data DescribeLayers = DescribeLayers'{_dlLayerIds :: Maybe [Text], _dlStackId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeLayers' smart constructor.
describeLayers :: DescribeLayers
describeLayers = DescribeLayers'{_dlLayerIds = Nothing, _dlStackId = Nothing};

-- | An array of layer IDs that specify the layers to be described. If you
-- omit this parameter, @DescribeLayers@ returns a description of every
-- layer in the specified stack.
dlLayerIds :: Lens' DescribeLayers [Text]
dlLayerIds = lens _dlLayerIds (\ s a -> s{_dlLayerIds = a}) . _Default;

-- | The stack ID.
dlStackId :: Lens' DescribeLayers (Maybe Text)
dlStackId = lens _dlStackId (\ s a -> s{_dlStackId = a});

instance AWSRequest DescribeLayers where
        type Sv DescribeLayers = OpsWorks
        type Rs DescribeLayers = DescribeLayersResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLayersResponse' <$>
                   (x .?> "Layers" .!@ mempty))

instance ToHeaders DescribeLayers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeLayers" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeLayers where
        toJSON DescribeLayers'{..}
          = object
              ["LayerIds" .= _dlLayerIds, "StackId" .= _dlStackId]

instance ToPath DescribeLayers where
        toPath = const "/"

instance ToQuery DescribeLayers where
        toQuery = const mempty

-- | /See:/ 'describeLayersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlrLayers'
newtype DescribeLayersResponse = DescribeLayersResponse'{_dlrLayers :: Maybe [Layer]} deriving (Eq, Read, Show)

-- | 'DescribeLayersResponse' smart constructor.
describeLayersResponse :: DescribeLayersResponse
describeLayersResponse = DescribeLayersResponse'{_dlrLayers = Nothing};

-- | An array of @Layer@ objects that describe the layers.
dlrLayers :: Lens' DescribeLayersResponse [Layer]
dlrLayers = lens _dlrLayers (\ s a -> s{_dlrLayers = a}) . _Default;
