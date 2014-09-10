{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified lifecycle hook. If there are any outstanding
-- lifecycle actions, they are completed first (ABANDON for launching
-- instances, CONTINUE for terminating instances).
module Network.AWS.AutoScaling
    (
    -- * Request
      DeleteLifecycleHook
    -- ** Request constructor
    , mkDeleteLifecycleHook
    -- ** Request lenses
    , dlhLifecycleHookName
    , dlhAutoScalingGroupName

    -- * Response
    , DeleteLifecycleHookResponse
    -- ** Response constructor
    , mkDeleteLifecycleHookResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

data DeleteLifecycleHook = DeleteLifecycleHook
    { _dlhLifecycleHookName :: !Text
    , _dlhAutoScalingGroupName :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLifecycleHook' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LifecycleHookName ::@ @Text@
--
-- * @AutoScalingGroupName ::@ @Text@
--
mkDeleteLifecycleHook :: Text -- ^ 'dlhLifecycleHookName'
                      -> Text -- ^ 'dlhAutoScalingGroupName'
                      -> DeleteLifecycleHook
mkDeleteLifecycleHook p1 p2 = DeleteLifecycleHook
    { _dlhLifecycleHookName = p1
    , _dlhAutoScalingGroupName = p2
    }

-- | The name of the lifecycle hook.
dlhLifecycleHookName :: Lens' DeleteLifecycleHook Text
dlhLifecycleHookName =
    lens _dlhLifecycleHookName (\s a -> s { _dlhLifecycleHookName = a })

-- | The name of the Auto Scaling group to which the lifecycle hook belongs.
dlhAutoScalingGroupName :: Lens' DeleteLifecycleHook Text
dlhAutoScalingGroupName =
    lens _dlhAutoScalingGroupName
         (\s a -> s { _dlhAutoScalingGroupName = a })

instance ToQuery DeleteLifecycleHook where
    toQuery = genericQuery def

-- | The output of the DeleteLifecycleHook action.
data DeleteLifecycleHookResponse = DeleteLifecycleHookResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLifecycleHookResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteLifecycleHookResponse :: DeleteLifecycleHookResponse
mkDeleteLifecycleHookResponse = DeleteLifecycleHookResponse

instance AWSRequest DeleteLifecycleHook where
    type Sv DeleteLifecycleHook = AutoScaling
    type Rs DeleteLifecycleHook = DeleteLifecycleHookResponse

    request = post "DeleteLifecycleHook"
    response _ = nullaryResponse DeleteLifecycleHookResponse
