{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DeleteLifecycleHook
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified lifecycle hook.
--
-- If there are any outstanding lifecycle actions, they are completed first ('ABANDON' for launching instances, 'CONTINUE' for terminating instances).
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteLifecycleHook.html>
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
    } deriving (Eq, Ord, Read, Show)

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

-- | The name of the Auto Scaling group for the lifecycle hook.
dlh1AutoScalingGroupName :: Lens' DeleteLifecycleHook Text
dlh1AutoScalingGroupName =
    lens _dlh1AutoScalingGroupName
        (\s a -> s { _dlh1AutoScalingGroupName = a })

-- | The name of the lifecycle hook.
dlh1LifecycleHookName :: Lens' DeleteLifecycleHook Text
dlh1LifecycleHookName =
    lens _dlh1LifecycleHookName (\s a -> s { _dlh1LifecycleHookName = a })

data DeleteLifecycleHookResponse = DeleteLifecycleHookResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteLifecycleHookResponse' constructor.
deleteLifecycleHookResponse :: DeleteLifecycleHookResponse
deleteLifecycleHookResponse = DeleteLifecycleHookResponse

instance ToPath DeleteLifecycleHook where
    toPath = const "/"

instance ToQuery DeleteLifecycleHook where
    toQuery DeleteLifecycleHook{..} = mconcat
        [ "AutoScalingGroupName" =? _dlh1AutoScalingGroupName
        , "LifecycleHookName"    =? _dlh1LifecycleHookName
        ]

instance ToHeaders DeleteLifecycleHook

instance AWSRequest DeleteLifecycleHook where
    type Sv DeleteLifecycleHook = AutoScaling
    type Rs DeleteLifecycleHook = DeleteLifecycleHookResponse

    request  = post "DeleteLifecycleHook"
    response = nullResponse DeleteLifecycleHookResponse
