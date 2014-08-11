{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.AutoScaling.V2011_01_01.DeleteScheduledAction where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteScheduledAction' request.
deleteScheduledAction :: Text -- ^ '_dsatScheduledActionName'
                      -> DeleteScheduledAction
deleteScheduledAction p1 = DeleteScheduledAction
    { _dsatScheduledActionName = p1
    , _dsatAutoScalingGroupName = Nothing
    }

data DeleteScheduledAction = DeleteScheduledAction
    { _dsatScheduledActionName :: Text
      -- ^ The name of the action you want to delete.
    , _dsatAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group.
    } deriving (Show, Generic)

makeLenses ''DeleteScheduledAction

instance ToQuery DeleteScheduledAction where
    toQuery = genericToQuery def

data DeleteScheduledActionResponse = DeleteScheduledActionResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteScheduledActionResponse

instance AWSRequest DeleteScheduledAction where
    type Sv DeleteScheduledAction = AutoScaling
    type Rs DeleteScheduledAction = DeleteScheduledActionResponse

    request = post "DeleteScheduledAction"
    response _ = nullaryResponse DeleteScheduledActionResponse
