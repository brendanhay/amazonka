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
-- Module      : Network.AWS.AutoScaling.DescribeAccountLimits
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current Auto Scaling resource limits for your AWS account.
--
-- For information about requesting an increase in these limits, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html AWS Service Limits>
-- in the /Amazon Web Services General Reference/.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAccountLimits.html AWS API Reference> for DescribeAccountLimits.
module Network.AWS.AutoScaling.DescribeAccountLimits
    (
    -- * Creating a Request
      describeAccountLimits
    , DescribeAccountLimits

    -- * Destructuring the Response
    , describeAccountLimitsResponse
    , DescribeAccountLimitsResponse
    -- * Response Lenses
    , dalrsMaxNumberOfAutoScalingGroups
    , dalrsMaxNumberOfLaunchConfigurations
    , dalrsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.AutoScaling.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAccountLimits' smart constructor.
data DescribeAccountLimits =
    DescribeAccountLimits'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAccountLimits' with the minimum fields required to make a request.
--
describeAccountLimits
    :: DescribeAccountLimits
describeAccountLimits = DescribeAccountLimits'

instance AWSRequest DescribeAccountLimits where
        type Rs DescribeAccountLimits =
             DescribeAccountLimitsResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper "DescribeAccountLimitsResult"
              (\ s h x ->
                 DescribeAccountLimitsResponse' <$>
                   (x .@? "MaxNumberOfAutoScalingGroups") <*>
                     (x .@? "MaxNumberOfLaunchConfigurations")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeAccountLimits where
        toHeaders = const mempty

instance ToPath DescribeAccountLimits where
        toPath = const "/"

instance ToQuery DescribeAccountLimits where
        toQuery
          = const
              (mconcat
                 ["Action" =: ("DescribeAccountLimits" :: ByteString),
                  "Version" =: ("2011-01-01" :: ByteString)])

-- | /See:/ 'describeAccountLimitsResponse' smart constructor.
data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse'
    { _dalrsMaxNumberOfAutoScalingGroups    :: !(Maybe Int)
    , _dalrsMaxNumberOfLaunchConfigurations :: !(Maybe Int)
    , _dalrsStatus                          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAccountLimitsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dalrsMaxNumberOfAutoScalingGroups'
--
-- * 'dalrsMaxNumberOfLaunchConfigurations'
--
-- * 'dalrsStatus'
describeAccountLimitsResponse
    :: Int -- ^ 'dalrsStatus'
    -> DescribeAccountLimitsResponse
describeAccountLimitsResponse pStatus_ =
    DescribeAccountLimitsResponse'
    { _dalrsMaxNumberOfAutoScalingGroups = Nothing
    , _dalrsMaxNumberOfLaunchConfigurations = Nothing
    , _dalrsStatus = pStatus_
    }

-- | The maximum number of groups allowed for your AWS account. The default
-- limit is 20 per region.
dalrsMaxNumberOfAutoScalingGroups :: Lens' DescribeAccountLimitsResponse (Maybe Int)
dalrsMaxNumberOfAutoScalingGroups = lens _dalrsMaxNumberOfAutoScalingGroups (\ s a -> s{_dalrsMaxNumberOfAutoScalingGroups = a});

-- | The maximum number of launch configurations allowed for your AWS
-- account. The default limit is 100 per region.
dalrsMaxNumberOfLaunchConfigurations :: Lens' DescribeAccountLimitsResponse (Maybe Int)
dalrsMaxNumberOfLaunchConfigurations = lens _dalrsMaxNumberOfLaunchConfigurations (\ s a -> s{_dalrsMaxNumberOfLaunchConfigurations = a});

-- | The response status code.
dalrsStatus :: Lens' DescribeAccountLimitsResponse Int
dalrsStatus = lens _dalrsStatus (\ s a -> s{_dalrsStatus = a});
