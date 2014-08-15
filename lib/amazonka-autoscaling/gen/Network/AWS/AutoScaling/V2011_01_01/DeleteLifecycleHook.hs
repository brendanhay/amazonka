{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

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
module Network.AWS.AutoScaling.V2011_01_01.DeleteLifecycleHook where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

data DeleteLifecycleHook = DeleteLifecycleHook
    { _dlhuLifecycleHookName :: Text
      -- ^ The name of the lifecycle hook.
    , _dlhuAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group to which the lifecycle hook
      -- belongs.
    } deriving (Show, Generic)

makeLenses ''DeleteLifecycleHook

instance ToQuery DeleteLifecycleHook where
    toQuery = genericQuery def

data DeleteLifecycleHookResponse = DeleteLifecycleHookResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteLifecycleHookResponse

instance AWSRequest DeleteLifecycleHook where
    type Sv DeleteLifecycleHook = AutoScaling
    type Rs DeleteLifecycleHook = DeleteLifecycleHookResponse

    request = post "DeleteLifecycleHook"
    response _ = nullaryResponse DeleteLifecycleHookResponse
