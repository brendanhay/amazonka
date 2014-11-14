{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Terminates the specified instance. Optionally, the desired group size can
-- be adjusted.
module Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
    (
    -- * Request
      TerminateInstanceInAutoScalingGroup
    -- ** Request constructor
    , terminateInstanceInAutoScalingGroup
    -- ** Request lenses
    , tiiasgInstanceId
    , tiiasgShouldDecrementDesiredCapacity

    -- * Response
    , TerminateInstanceInAutoScalingGroupResponse
    -- ** Response constructor
    , terminateInstanceInAutoScalingGroupResponse
    -- ** Response lenses
    , tiiasgrActivity
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroup
    { _tiiasgInstanceId                     :: Text
    , _tiiasgShouldDecrementDesiredCapacity :: Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'TerminateInstanceInAutoScalingGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tiiasgInstanceId' @::@ 'Text'
--
-- * 'tiiasgShouldDecrementDesiredCapacity' @::@ 'Bool'
--
terminateInstanceInAutoScalingGroup :: Text -- ^ 'tiiasgInstanceId'
                                    -> Bool -- ^ 'tiiasgShouldDecrementDesiredCapacity'
                                    -> TerminateInstanceInAutoScalingGroup
terminateInstanceInAutoScalingGroup p1 p2 = TerminateInstanceInAutoScalingGroup
    { _tiiasgInstanceId                     = p1
    , _tiiasgShouldDecrementDesiredCapacity = p2
    }

-- | The ID of the Amazon EC2 instance to be terminated.
tiiasgInstanceId :: Lens' TerminateInstanceInAutoScalingGroup Text
tiiasgInstanceId = lens _tiiasgInstanceId (\s a -> s { _tiiasgInstanceId = a })

-- | Specifies whether (true) or not (false) terminating this instance should
-- also decrement the size of the AutoScalingGroup.
tiiasgShouldDecrementDesiredCapacity :: Lens' TerminateInstanceInAutoScalingGroup Bool
tiiasgShouldDecrementDesiredCapacity =
    lens _tiiasgShouldDecrementDesiredCapacity
        (\s a -> s { _tiiasgShouldDecrementDesiredCapacity = a })

instance ToQuery TerminateInstanceInAutoScalingGroup

instance ToPath TerminateInstanceInAutoScalingGroup where
    toPath = const "/"

newtype TerminateInstanceInAutoScalingGroupResponse = TerminateInstanceInAutoScalingGroupResponse
    { _tiiasgrActivity :: Maybe Activity
    } deriving (Eq, Show, Generic)

-- | 'TerminateInstanceInAutoScalingGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tiiasgrActivity' @::@ 'Maybe' 'Activity'
--
terminateInstanceInAutoScalingGroupResponse :: TerminateInstanceInAutoScalingGroupResponse
terminateInstanceInAutoScalingGroupResponse = TerminateInstanceInAutoScalingGroupResponse
    { _tiiasgrActivity = Nothing
    }

-- | A scaling Activity.
tiiasgrActivity :: Lens' TerminateInstanceInAutoScalingGroupResponse (Maybe Activity)
tiiasgrActivity = lens _tiiasgrActivity (\s a -> s { _tiiasgrActivity = a })

instance AWSRequest TerminateInstanceInAutoScalingGroup where
    type Sv TerminateInstanceInAutoScalingGroup = AutoScaling
    type Rs TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroupResponse

    request  = post "TerminateInstanceInAutoScalingGroup"
    response = xmlResponse $ \h x -> TerminateInstanceInAutoScalingGroupResponse
        <$> x %| "Activity"
