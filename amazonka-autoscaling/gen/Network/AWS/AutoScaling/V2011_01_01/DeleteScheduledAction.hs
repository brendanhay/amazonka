{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DeleteScheduledAction
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a scheduled action previously created using the
-- PutScheduledUpdateGroupAction.
module Network.AWS.AutoScaling.V2011_01_01.DeleteScheduledAction
    (
    -- * Request
      DeleteScheduledAction
    -- ** Request constructor
    , deleteScheduledAction
    -- ** Request lenses
    , dsatScheduledActionName
    , dsatAutoScalingGroupName

    -- * Response
    , DeleteScheduledActionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteScheduledAction' request.
deleteScheduledAction :: Text -- ^ 'dsatScheduledActionName'
                      -> DeleteScheduledAction
deleteScheduledAction p1 = DeleteScheduledAction
    { _dsatScheduledActionName = p1
    , _dsatAutoScalingGroupName = Nothing
    }
{-# INLINE deleteScheduledAction #-}

data DeleteScheduledAction = DeleteScheduledAction
    { _dsatScheduledActionName :: Text
      -- ^ The name of the action you want to delete.
    , _dsatAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group.
    } deriving (Show, Generic)

-- | The name of the action you want to delete.
dsatScheduledActionName :: Lens' DeleteScheduledAction (Text)
dsatScheduledActionName f x =
    f (_dsatScheduledActionName x)
        <&> \y -> x { _dsatScheduledActionName = y }
{-# INLINE dsatScheduledActionName #-}

-- | The name of the Auto Scaling group.
dsatAutoScalingGroupName :: Lens' DeleteScheduledAction (Maybe Text)
dsatAutoScalingGroupName f x =
    f (_dsatAutoScalingGroupName x)
        <&> \y -> x { _dsatAutoScalingGroupName = y }
{-# INLINE dsatAutoScalingGroupName #-}

instance ToQuery DeleteScheduledAction where
    toQuery = genericQuery def

data DeleteScheduledActionResponse = DeleteScheduledActionResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteScheduledAction where
    type Sv DeleteScheduledAction = AutoScaling
    type Rs DeleteScheduledAction = DeleteScheduledActionResponse

    request = post "DeleteScheduledAction"
    response _ = nullaryResponse DeleteScheduledActionResponse
