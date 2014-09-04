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
    , mkDeleteScheduledActionType
    -- ** Request lenses
    , dsatAutoScalingGroupName
    , dsatScheduledActionName

    -- * Response
    , DeleteScheduledActionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteScheduledAction' request.
mkDeleteScheduledActionType :: Text -- ^ 'dsatScheduledActionName'
                            -> DeleteScheduledAction
mkDeleteScheduledActionType p1 = DeleteScheduledAction
    { _dsatAutoScalingGroupName = Nothing
    , _dsatScheduledActionName = p2
    }
{-# INLINE mkDeleteScheduledActionType #-}

data DeleteScheduledAction = DeleteScheduledAction
    { _dsatAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group.
    , _dsatScheduledActionName :: Text
      -- ^ The name of the action you want to delete.
    } deriving (Show, Generic)

-- | The name of the Auto Scaling group.
dsatAutoScalingGroupName :: Lens' DeleteScheduledAction (Maybe Text)
dsatAutoScalingGroupName = lens _dsatAutoScalingGroupName (\s a -> s { _dsatAutoScalingGroupName = a })
{-# INLINE dsatAutoScalingGroupName #-}

-- | The name of the action you want to delete.
dsatScheduledActionName :: Lens' DeleteScheduledAction (Text)
dsatScheduledActionName = lens _dsatScheduledActionName (\s a -> s { _dsatScheduledActionName = a })
{-# INLINE dsatScheduledActionName #-}

instance ToQuery DeleteScheduledAction where
    toQuery = genericQuery def

data DeleteScheduledActionResponse = DeleteScheduledActionResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteScheduledAction where
    type Sv DeleteScheduledAction = AutoScaling
    type Rs DeleteScheduledAction = DeleteScheduledActionResponse

    request = post "DeleteScheduledAction"
    response _ = nullaryResponse DeleteScheduledActionResponse
