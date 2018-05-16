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
-- Module      : Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes load-based auto scaling configurations for specified layers.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
    (
    -- * Creating a Request
      describeLoadBasedAutoScaling
    , DescribeLoadBasedAutoScaling
    -- * Request Lenses
    , dlbasLayerIds

    -- * Destructuring the Response
    , describeLoadBasedAutoScalingResponse
    , DescribeLoadBasedAutoScalingResponse
    -- * Response Lenses
    , dlbasrsLoadBasedAutoScalingConfigurations
    , dlbasrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLoadBasedAutoScaling' smart constructor.
newtype DescribeLoadBasedAutoScaling = DescribeLoadBasedAutoScaling'
  { _dlbasLayerIds :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLoadBasedAutoScaling' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbasLayerIds' - An array of layer IDs.
describeLoadBasedAutoScaling
    :: DescribeLoadBasedAutoScaling
describeLoadBasedAutoScaling =
  DescribeLoadBasedAutoScaling' {_dlbasLayerIds = mempty}


-- | An array of layer IDs.
dlbasLayerIds :: Lens' DescribeLoadBasedAutoScaling [Text]
dlbasLayerIds = lens _dlbasLayerIds (\ s a -> s{_dlbasLayerIds = a}) . _Coerce

instance AWSRequest DescribeLoadBasedAutoScaling
         where
        type Rs DescribeLoadBasedAutoScaling =
             DescribeLoadBasedAutoScalingResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLoadBasedAutoScalingResponse' <$>
                   (x .?> "LoadBasedAutoScalingConfigurations" .!@
                      mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeLoadBasedAutoScaling where

instance NFData DescribeLoadBasedAutoScaling where

instance ToHeaders DescribeLoadBasedAutoScaling where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeLoadBasedAutoScaling" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeLoadBasedAutoScaling where
        toJSON DescribeLoadBasedAutoScaling'{..}
          = object
              (catMaybes [Just ("LayerIds" .= _dlbasLayerIds)])

instance ToPath DescribeLoadBasedAutoScaling where
        toPath = const "/"

instance ToQuery DescribeLoadBasedAutoScaling where
        toQuery = const mempty

-- | Contains the response to a @DescribeLoadBasedAutoScaling@ request.
--
--
--
-- /See:/ 'describeLoadBasedAutoScalingResponse' smart constructor.
data DescribeLoadBasedAutoScalingResponse = DescribeLoadBasedAutoScalingResponse'
  { _dlbasrsLoadBasedAutoScalingConfigurations :: !(Maybe [LoadBasedAutoScalingConfiguration])
  , _dlbasrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLoadBasedAutoScalingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbasrsLoadBasedAutoScalingConfigurations' - An array of @LoadBasedAutoScalingConfiguration@ objects that describe each layer's configuration.
--
-- * 'dlbasrsResponseStatus' - -- | The response status code.
describeLoadBasedAutoScalingResponse
    :: Int -- ^ 'dlbasrsResponseStatus'
    -> DescribeLoadBasedAutoScalingResponse
describeLoadBasedAutoScalingResponse pResponseStatus_ =
  DescribeLoadBasedAutoScalingResponse'
    { _dlbasrsLoadBasedAutoScalingConfigurations = Nothing
    , _dlbasrsResponseStatus = pResponseStatus_
    }


-- | An array of @LoadBasedAutoScalingConfiguration@ objects that describe each layer's configuration.
dlbasrsLoadBasedAutoScalingConfigurations :: Lens' DescribeLoadBasedAutoScalingResponse [LoadBasedAutoScalingConfiguration]
dlbasrsLoadBasedAutoScalingConfigurations = lens _dlbasrsLoadBasedAutoScalingConfigurations (\ s a -> s{_dlbasrsLoadBasedAutoScalingConfigurations = a}) . _Default . _Coerce

-- | -- | The response status code.
dlbasrsResponseStatus :: Lens' DescribeLoadBasedAutoScalingResponse Int
dlbasrsResponseStatus = lens _dlbasrsResponseStatus (\ s a -> s{_dlbasrsResponseStatus = a})

instance NFData DescribeLoadBasedAutoScalingResponse
         where
