{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.OpsWorks.StopInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Stops a specified instance. When you stop a standard instance, the data
-- disappears and must be reinstalled when you restart the instance. You can
-- stop an Amazon EBS-backed instance without losing data. For more
-- information, see Starting, Stopping, and Rebooting Instances. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
module Network.AWS.OpsWorks.StopInstance
    (
    -- * Request
      StopInstance
    -- ** Request constructor
    , stopInstance
    -- ** Request lenses
    , siInstanceId

    -- * Response
    , StopInstanceResponse
    -- ** Response constructor
    , stopInstanceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

newtype StopInstance = StopInstance
    { _siInstanceId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'StopInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'siInstanceId' @::@ 'Text'
--
stopInstance :: Text -- ^ 'siInstanceId'
             -> StopInstance
stopInstance p1 = StopInstance
    { _siInstanceId = p1
    }

-- | The instance ID.
siInstanceId :: Lens' StopInstance Text
siInstanceId = lens _siInstanceId (\s a -> s { _siInstanceId = a })

instance ToPath StopInstance where
    toPath = const "/"

instance ToQuery StopInstance where
    toQuery = const mempty

instance ToHeaders StopInstance

instance ToBody StopInstance where
    toBody = toBody . encode . _siInstanceId

data StopInstanceResponse = StopInstanceResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'StopInstanceResponse' constructor.
stopInstanceResponse :: StopInstanceResponse
stopInstanceResponse = StopInstanceResponse

-- FromJSON

instance AWSRequest StopInstance where
    type Sv StopInstance = OpsWorks
    type Rs StopInstance = StopInstanceResponse

    request  = post'
    response = nullaryResponse StopInstanceResponse
