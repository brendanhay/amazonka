{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeLayers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of one or more layers in a specified stack.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DescribeLayers
    (
    -- * Creating a Request
      describeLayers
    , DescribeLayers
    -- * Request Lenses
    , dlLayerIds
    , dlStackId

    -- * Destructuring the Response
    , describeLayersResponse
    , DescribeLayersResponse
    -- * Response Lenses
    , dlrsLayers
    , dlrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLayers' smart constructor.
data DescribeLayers = DescribeLayers'
  { _dlLayerIds :: !(Maybe [Text])
  , _dlStackId  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLayers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlLayerIds' - An array of layer IDs that specify the layers to be described. If you omit this parameter, @DescribeLayers@ returns a description of every layer in the specified stack.
--
-- * 'dlStackId' - The stack ID.
describeLayers
    :: DescribeLayers
describeLayers = DescribeLayers' {_dlLayerIds = Nothing, _dlStackId = Nothing}


-- | An array of layer IDs that specify the layers to be described. If you omit this parameter, @DescribeLayers@ returns a description of every layer in the specified stack.
dlLayerIds :: Lens' DescribeLayers [Text]
dlLayerIds = lens _dlLayerIds (\ s a -> s{_dlLayerIds = a}) . _Default . _Coerce

-- | The stack ID.
dlStackId :: Lens' DescribeLayers (Maybe Text)
dlStackId = lens _dlStackId (\ s a -> s{_dlStackId = a})

instance AWSRequest DescribeLayers where
        type Rs DescribeLayers = DescribeLayersResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLayersResponse' <$>
                   (x .?> "Layers" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable DescribeLayers where

instance NFData DescribeLayers where

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
              (catMaybes
                 [("LayerIds" .=) <$> _dlLayerIds,
                  ("StackId" .=) <$> _dlStackId])

instance ToPath DescribeLayers where
        toPath = const "/"

instance ToQuery DescribeLayers where
        toQuery = const mempty

-- | Contains the response to a @DescribeLayers@ request.
--
--
--
-- /See:/ 'describeLayersResponse' smart constructor.
data DescribeLayersResponse = DescribeLayersResponse'
  { _dlrsLayers         :: !(Maybe [Layer])
  , _dlrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLayersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlrsLayers' - An array of @Layer@ objects that describe the layers.
--
-- * 'dlrsResponseStatus' - -- | The response status code.
describeLayersResponse
    :: Int -- ^ 'dlrsResponseStatus'
    -> DescribeLayersResponse
describeLayersResponse pResponseStatus_ =
  DescribeLayersResponse'
    {_dlrsLayers = Nothing, _dlrsResponseStatus = pResponseStatus_}


-- | An array of @Layer@ objects that describe the layers.
dlrsLayers :: Lens' DescribeLayersResponse [Layer]
dlrsLayers = lens _dlrsLayers (\ s a -> s{_dlrsLayers = a}) . _Default . _Coerce

-- | -- | The response status code.
dlrsResponseStatus :: Lens' DescribeLayersResponse Int
dlrsResponseStatus = lens _dlrsResponseStatus (\ s a -> s{_dlrsResponseStatus = a})

instance NFData DescribeLayersResponse where
