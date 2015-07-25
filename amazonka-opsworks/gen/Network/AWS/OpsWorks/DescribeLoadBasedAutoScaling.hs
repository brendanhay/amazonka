{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes load-based auto scaling configurations for specified layers.
--
-- You must specify at least one of the parameters.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeLoadBasedAutoScaling.html>
module Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
    (
    -- * Request
      DescribeLoadBasedAutoScaling
    -- ** Request constructor
    , describeLoadBasedAutoScaling
    -- ** Request lenses
    , dlbasLayerIds

    -- * Response
    , DescribeLoadBasedAutoScalingResponse
    -- ** Response constructor
    , describeLoadBasedAutoScalingResponse
    -- ** Response lenses
    , dlbasrsLoadBasedAutoScalingConfigurations
    , dlbasrsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLoadBasedAutoScaling' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbasLayerIds'
newtype DescribeLoadBasedAutoScaling = DescribeLoadBasedAutoScaling'
    { _dlbasLayerIds :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLoadBasedAutoScaling' smart constructor.
describeLoadBasedAutoScaling :: DescribeLoadBasedAutoScaling
describeLoadBasedAutoScaling =
    DescribeLoadBasedAutoScaling'
    { _dlbasLayerIds = mempty
    }

-- | An array of layer IDs.
dlbasLayerIds :: Lens' DescribeLoadBasedAutoScaling [Text]
dlbasLayerIds = lens _dlbasLayerIds (\ s a -> s{_dlbasLayerIds = a}) . _Coerce;

instance AWSRequest DescribeLoadBasedAutoScaling
         where
        type Sv DescribeLoadBasedAutoScaling = OpsWorks
        type Rs DescribeLoadBasedAutoScaling =
             DescribeLoadBasedAutoScalingResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLoadBasedAutoScalingResponse' <$>
                   (x .?> "LoadBasedAutoScalingConfigurations" .!@
                      mempty)
                     <*> (pure (fromEnum s)))

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
          = object ["LayerIds" .= _dlbasLayerIds]

instance ToPath DescribeLoadBasedAutoScaling where
        toPath = const "/"

instance ToQuery DescribeLoadBasedAutoScaling where
        toQuery = const mempty

-- | Contains the response to a @DescribeLoadBasedAutoScaling@ request.
--
-- /See:/ 'describeLoadBasedAutoScalingResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbasrsLoadBasedAutoScalingConfigurations'
--
-- * 'dlbasrsStatus'
data DescribeLoadBasedAutoScalingResponse = DescribeLoadBasedAutoScalingResponse'
    { _dlbasrsLoadBasedAutoScalingConfigurations :: !(Maybe [LoadBasedAutoScalingConfiguration])
    , _dlbasrsStatus                             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLoadBasedAutoScalingResponse' smart constructor.
describeLoadBasedAutoScalingResponse :: Int -> DescribeLoadBasedAutoScalingResponse
describeLoadBasedAutoScalingResponse pStatus_ =
    DescribeLoadBasedAutoScalingResponse'
    { _dlbasrsLoadBasedAutoScalingConfigurations = Nothing
    , _dlbasrsStatus = pStatus_
    }

-- | An array of @LoadBasedAutoScalingConfiguration@ objects that describe
-- each layer\'s configuration.
dlbasrsLoadBasedAutoScalingConfigurations :: Lens' DescribeLoadBasedAutoScalingResponse [LoadBasedAutoScalingConfiguration]
dlbasrsLoadBasedAutoScalingConfigurations = lens _dlbasrsLoadBasedAutoScalingConfigurations (\ s a -> s{_dlbasrsLoadBasedAutoScalingConfigurations = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
dlbasrsStatus :: Lens' DescribeLoadBasedAutoScalingResponse Int
dlbasrsStatus = lens _dlbasrsStatus (\ s a -> s{_dlbasrsStatus = a});
