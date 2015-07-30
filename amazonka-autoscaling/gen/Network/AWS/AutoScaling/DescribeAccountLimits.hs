{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeAccountLimits
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the current Auto Scaling resource limits for your AWS account.
--
-- For information about requesting an increase in these limits, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html AWS Service Limits>
-- in the /Amazon Web Services General Reference/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAccountLimits.html>
module Network.AWS.AutoScaling.DescribeAccountLimits
    (
    -- * Request
      DescribeAccountLimits
    -- ** Request constructor
    , describeAccountLimits

    -- * Response
    , DescribeAccountLimitsResponse
    -- ** Response constructor
    , describeAccountLimitsResponse
    -- ** Response lenses
    , dalrsMaxNumberOfLaunchConfigurations
    , dalrsMaxNumberOfAutoScalingGroups
    , dalrsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAccountLimits' smart constructor.
data DescribeAccountLimits =
    DescribeAccountLimits'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAccountLimits' smart constructor.
describeAccountLimits :: DescribeAccountLimits
describeAccountLimits = DescribeAccountLimits'

instance AWSRequest DescribeAccountLimits where
        type Sv DescribeAccountLimits = AutoScaling
        type Rs DescribeAccountLimits =
             DescribeAccountLimitsResponse
        request = postQuery
        response
          = receiveXMLWrapper "DescribeAccountLimitsResult"
              (\ s h x ->
                 DescribeAccountLimitsResponse' <$>
                   (x .@? "MaxNumberOfLaunchConfigurations") <*>
                     (x .@? "MaxNumberOfAutoScalingGroups")
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dalrsMaxNumberOfLaunchConfigurations'
--
-- * 'dalrsMaxNumberOfAutoScalingGroups'
--
-- * 'dalrsStatus'
data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse'
    { _dalrsMaxNumberOfLaunchConfigurations :: !(Maybe Int)
    , _dalrsMaxNumberOfAutoScalingGroups    :: !(Maybe Int)
    , _dalrsStatus                          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAccountLimitsResponse' smart constructor.
describeAccountLimitsResponse :: Int -> DescribeAccountLimitsResponse
describeAccountLimitsResponse pStatus_ =
    DescribeAccountLimitsResponse'
    { _dalrsMaxNumberOfLaunchConfigurations = Nothing
    , _dalrsMaxNumberOfAutoScalingGroups = Nothing
    , _dalrsStatus = pStatus_
    }

-- | The maximum number of launch configurations allowed for your AWS
-- account. The default limit is 100 per region.
dalrsMaxNumberOfLaunchConfigurations :: Lens' DescribeAccountLimitsResponse (Maybe Int)
dalrsMaxNumberOfLaunchConfigurations = lens _dalrsMaxNumberOfLaunchConfigurations (\ s a -> s{_dalrsMaxNumberOfLaunchConfigurations = a});

-- | The maximum number of groups allowed for your AWS account. The default
-- limit is 20 per region.
dalrsMaxNumberOfAutoScalingGroups :: Lens' DescribeAccountLimitsResponse (Maybe Int)
dalrsMaxNumberOfAutoScalingGroups = lens _dalrsMaxNumberOfAutoScalingGroups (\ s a -> s{_dalrsMaxNumberOfAutoScalingGroups = a});

-- | FIXME: Undocumented member.
dalrsStatus :: Lens' DescribeAccountLimitsResponse Int
dalrsStatus = lens _dalrsStatus (\ s a -> s{_dalrsStatus = a});
