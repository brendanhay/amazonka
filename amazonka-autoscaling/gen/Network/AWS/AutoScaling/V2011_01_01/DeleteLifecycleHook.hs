{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DeleteLifecycleHook
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
module Network.AWS.AutoScaling.V2011_01_01.DeleteLifecycleHook
    (
    -- * Request
      DeleteLifecycleHook
    -- ** Request constructor
    , mkDeleteLifecycleHookType
    -- ** Request lenses
    , dlhtLifecycleHookName
    , dlhtAutoScalingGroupName

    -- * Response
    , DeleteLifecycleHookResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLifecycleHook' request.
mkDeleteLifecycleHookType :: Text -- ^ 'dlhtLifecycleHookName'
                          -> Text -- ^ 'dlhtAutoScalingGroupName'
                          -> DeleteLifecycleHook
mkDeleteLifecycleHookType p1 p2 = DeleteLifecycleHook
    { _dlhtLifecycleHookName = p1
    , _dlhtAutoScalingGroupName = p2
    }
{-# INLINE mkDeleteLifecycleHookType #-}

data DeleteLifecycleHook = DeleteLifecycleHook
    { _dlhtLifecycleHookName :: Text
      -- ^ The name of the lifecycle hook.
    , _dlhtAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group to which the lifecycle hook
      -- belongs.
    } deriving (Show, Generic)

-- | The name of the lifecycle hook.
dlhtLifecycleHookName :: Lens' DeleteLifecycleHook (Text)
dlhtLifecycleHookName = lens _dlhtLifecycleHookName (\s a -> s { _dlhtLifecycleHookName = a })
{-# INLINE dlhtLifecycleHookName #-}

-- | The name of the Auto Scaling group to which the lifecycle hook belongs.
dlhtAutoScalingGroupName :: Lens' DeleteLifecycleHook (Text)
dlhtAutoScalingGroupName = lens _dlhtAutoScalingGroupName (\s a -> s { _dlhtAutoScalingGroupName = a })
{-# INLINE dlhtAutoScalingGroupName #-}

instance ToQuery DeleteLifecycleHook where
    toQuery = genericQuery def

    deriving (Eq, Show, Generic)

instance AWSRequest DeleteLifecycleHook where
    type Sv DeleteLifecycleHook = AutoScaling
    type Rs DeleteLifecycleHook = DeleteLifecycleHookResponse

    request = post "DeleteLifecycleHook"
    response _ = nullaryResponse DeleteLifecycleHookResponse
