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
    , deleteLifecycleHook
    -- ** Request lenses
    , dlhtLifecycleHookName
    , dlhtAutoScalingGroupName

    -- * Response
    , DeleteLifecycleHookResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteLifecycleHook' request.
deleteLifecycleHook :: Text -- ^ 'dlhtLifecycleHookName'
                    -> Text -- ^ 'dlhtAutoScalingGroupName'
                    -> DeleteLifecycleHook
deleteLifecycleHook p1 p2 = DeleteLifecycleHook
    { _dlhtLifecycleHookName = p1
    , _dlhtAutoScalingGroupName = p2
    }

data DeleteLifecycleHook = DeleteLifecycleHook
    { _dlhtLifecycleHookName :: Text
      -- ^ The name of the lifecycle hook.
    , _dlhtAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group to which the lifecycle hook
      -- belongs.
    } deriving (Show, Generic)

-- | The name of the lifecycle hook.
dlhtLifecycleHookName
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteLifecycleHook
    -> f DeleteLifecycleHook
dlhtLifecycleHookName f x =
    (\y -> x { _dlhtLifecycleHookName = y })
       <$> f (_dlhtLifecycleHookName x)
{-# INLINE dlhtLifecycleHookName #-}

-- | The name of the Auto Scaling group to which the lifecycle hook belongs.
dlhtAutoScalingGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteLifecycleHook
    -> f DeleteLifecycleHook
dlhtAutoScalingGroupName f x =
    (\y -> x { _dlhtAutoScalingGroupName = y })
       <$> f (_dlhtAutoScalingGroupName x)
{-# INLINE dlhtAutoScalingGroupName #-}

instance ToQuery DeleteLifecycleHook where
    toQuery = genericQuery def

data DeleteLifecycleHookResponse = DeleteLifecycleHookResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteLifecycleHook where
    type Sv DeleteLifecycleHook = AutoScaling
    type Rs DeleteLifecycleHook = DeleteLifecycleHookResponse

    request = post "DeleteLifecycleHook"
    response _ = nullaryResponse DeleteLifecycleHookResponse
