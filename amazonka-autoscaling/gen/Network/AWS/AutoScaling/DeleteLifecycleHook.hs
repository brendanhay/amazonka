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

-- Module      : Network.AWS.AutoScaling.DeleteLifecycleHook
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
module Network.AWS.AutoScaling.DeleteLifecycleHook
    (
    -- * Request
      DeleteLifecycleHookType
    -- ** Request constructor
    , deleteLifecycleHookType
    -- ** Request lenses
    , dlhtAutoScalingGroupName
    , dlhtLifecycleHookName

    -- * Response
    , DeleteLifecycleHookResponse
    -- ** Response constructor
    , deleteLifecycleHookResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DeleteLifecycleHookType = DeleteLifecycleHookType
    { _dlhtAutoScalingGroupName :: Text
    , _dlhtLifecycleHookName    :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteLifecycleHookType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlhtAutoScalingGroupName' @::@ 'Text'
--
-- * 'dlhtLifecycleHookName' @::@ 'Text'
--
deleteLifecycleHookType :: Text -- ^ 'dlhtLifecycleHookName'
                        -> Text -- ^ 'dlhtAutoScalingGroupName'
                        -> DeleteLifecycleHookType
deleteLifecycleHookType p1 p2 = DeleteLifecycleHookType
    { _dlhtLifecycleHookName    = p1
    , _dlhtAutoScalingGroupName = p2
    }

-- | The name of the Auto Scaling group to which the lifecycle hook belongs.
dlhtAutoScalingGroupName :: Lens' DeleteLifecycleHookType Text
dlhtAutoScalingGroupName =
    lens _dlhtAutoScalingGroupName
        (\s a -> s { _dlhtAutoScalingGroupName = a })

-- | The name of the lifecycle hook.
dlhtLifecycleHookName :: Lens' DeleteLifecycleHookType Text
dlhtLifecycleHookName =
    lens _dlhtLifecycleHookName (\s a -> s { _dlhtLifecycleHookName = a })

instance ToPath DeleteLifecycleHookType where
    toPath = const "/"

instance ToQuery DeleteLifecycleHookType

data DeleteLifecycleHookResponse = DeleteLifecycleHookResponse

-- | 'DeleteLifecycleHookResponse' constructor.
deleteLifecycleHookResponse :: DeleteLifecycleHookResponse
deleteLifecycleHookResponse = DeleteLifecycleHookResponse

instance AWSRequest DeleteLifecycleHookType where
    type Sv DeleteLifecycleHookType = AutoScaling
    type Rs DeleteLifecycleHookType = DeleteLifecycleHookResponse

    request  = post "DeleteLifecycleHook"
    response = const (nullaryResponse DeleteLifecycleHookResponse)
