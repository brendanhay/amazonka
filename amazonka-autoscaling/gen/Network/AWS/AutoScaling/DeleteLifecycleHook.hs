{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

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
      DeleteLifecycleHook
    -- ** Request constructor
    , deleteLifecycleHook
    -- ** Request lenses
    , dlh1AutoScalingGroupName
    , dlh1LifecycleHookName

    -- * Response
    , DeleteLifecycleHookResponse
    -- ** Response constructor
    , deleteLifecycleHookResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DeleteLifecycleHook = DeleteLifecycleHook
    { _dlh1AutoScalingGroupName :: Text
    , _dlh1LifecycleHookName    :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteLifecycleHook' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlh1AutoScalingGroupName' @::@ 'Text'
--
-- * 'dlh1LifecycleHookName' @::@ 'Text'
--
deleteLifecycleHook :: Text -- ^ 'dlh1LifecycleHookName'
                    -> Text -- ^ 'dlh1AutoScalingGroupName'
                    -> DeleteLifecycleHook
deleteLifecycleHook p1 p2 = DeleteLifecycleHook
    { _dlh1LifecycleHookName    = p1
    , _dlh1AutoScalingGroupName = p2
    }

-- | The name of the Auto Scaling group to which the lifecycle hook belongs.
dlh1AutoScalingGroupName :: Lens' DeleteLifecycleHook Text
dlh1AutoScalingGroupName =
    lens _dlh1AutoScalingGroupName
        (\s a -> s { _dlh1AutoScalingGroupName = a })

-- | The name of the lifecycle hook.
dlh1LifecycleHookName :: Lens' DeleteLifecycleHook Text
dlh1LifecycleHookName =
    lens _dlh1LifecycleHookName (\s a -> s { _dlh1LifecycleHookName = a })

instance ToQuery DeleteLifecycleHook

instance ToPath DeleteLifecycleHook where
    toPath = const "/"

data DeleteLifecycleHookResponse = DeleteLifecycleHookResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteLifecycleHookResponse' constructor.
deleteLifecycleHookResponse :: DeleteLifecycleHookResponse
deleteLifecycleHookResponse = DeleteLifecycleHookResponse

instance AWSRequest DeleteLifecycleHook where
    type Sv DeleteLifecycleHook = AutoScaling
    type Rs DeleteLifecycleHook = DeleteLifecycleHookResponse

    request  = post "DeleteLifecycleHook"
    response = nullaryResponse DeleteLifecycleHookResponse
