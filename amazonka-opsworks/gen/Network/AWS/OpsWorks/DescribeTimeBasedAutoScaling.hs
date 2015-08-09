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
-- Module      : Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes time-based auto scaling configurations for specified
-- instances.
--
-- You must specify at least one of the parameters.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeTimeBasedAutoScaling.html AWS API Reference> for DescribeTimeBasedAutoScaling.
module Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling
    (
    -- * Creating a Request
      DescribeTimeBasedAutoScaling
    , describeTimeBasedAutoScaling
    -- * Request Lenses
    , dtbasInstanceIds

    -- * Destructuring the Response
    , DescribeTimeBasedAutoScalingResponse
    , describeTimeBasedAutoScalingResponse
    -- * Response Lenses
    , dtbasrsTimeBasedAutoScalingConfigurations
    , dtbasrsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.OpsWorks.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeTimeBasedAutoScaling' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtbasInstanceIds'
newtype DescribeTimeBasedAutoScaling = DescribeTimeBasedAutoScaling'
    { _dtbasInstanceIds :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTimeBasedAutoScaling' smart constructor.
describeTimeBasedAutoScaling :: DescribeTimeBasedAutoScaling
describeTimeBasedAutoScaling =
    DescribeTimeBasedAutoScaling'
    { _dtbasInstanceIds = mempty
    }

-- | An array of instance IDs.
dtbasInstanceIds :: Lens' DescribeTimeBasedAutoScaling [Text]
dtbasInstanceIds = lens _dtbasInstanceIds (\ s a -> s{_dtbasInstanceIds = a}) . _Coerce;

instance AWSRequest DescribeTimeBasedAutoScaling
         where
        type Sv DescribeTimeBasedAutoScaling = OpsWorks
        type Rs DescribeTimeBasedAutoScaling =
             DescribeTimeBasedAutoScalingResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTimeBasedAutoScalingResponse' <$>
                   (x .?> "TimeBasedAutoScalingConfigurations" .!@
                      mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeTimeBasedAutoScaling where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeTimeBasedAutoScaling" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTimeBasedAutoScaling where
        toJSON DescribeTimeBasedAutoScaling'{..}
          = object ["InstanceIds" .= _dtbasInstanceIds]

instance ToPath DescribeTimeBasedAutoScaling where
        toPath = const "/"

instance ToQuery DescribeTimeBasedAutoScaling where
        toQuery = const mempty

-- | Contains the response to a @DescribeTimeBasedAutoScaling@ request.
--
-- /See:/ 'describeTimeBasedAutoScalingResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtbasrsTimeBasedAutoScalingConfigurations'
--
-- * 'dtbasrsStatus'
data DescribeTimeBasedAutoScalingResponse = DescribeTimeBasedAutoScalingResponse'
    { _dtbasrsTimeBasedAutoScalingConfigurations :: !(Maybe [TimeBasedAutoScalingConfiguration])
    , _dtbasrsStatus                             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTimeBasedAutoScalingResponse' smart constructor.
describeTimeBasedAutoScalingResponse :: Int -> DescribeTimeBasedAutoScalingResponse
describeTimeBasedAutoScalingResponse pStatus_ =
    DescribeTimeBasedAutoScalingResponse'
    { _dtbasrsTimeBasedAutoScalingConfigurations = Nothing
    , _dtbasrsStatus = pStatus_
    }

-- | An array of @TimeBasedAutoScalingConfiguration@ objects that describe
-- the configuration for the specified instances.
dtbasrsTimeBasedAutoScalingConfigurations :: Lens' DescribeTimeBasedAutoScalingResponse [TimeBasedAutoScalingConfiguration]
dtbasrsTimeBasedAutoScalingConfigurations = lens _dtbasrsTimeBasedAutoScalingConfigurations (\ s a -> s{_dtbasrsTimeBasedAutoScalingConfigurations = a}) . _Default . _Coerce;

-- | Undocumented member.
dtbasrsStatus :: Lens' DescribeTimeBasedAutoScalingResponse Int
dtbasrsStatus = lens _dtbasrsStatus (\ s a -> s{_dtbasrsStatus = a});
