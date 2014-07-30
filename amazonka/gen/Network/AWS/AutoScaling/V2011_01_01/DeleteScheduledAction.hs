{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.AutoScaling.V2011_01_01.DeleteScheduledAction where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.AutoScaling.V2011_01_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

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
    } deriving (Generic)

instance ToQuery DeleteScheduledAction where
    toQuery = genericToQuery def

instance AWSRequest DeleteScheduledAction where
    type Sv DeleteScheduledAction = AutoScaling
    type Rs DeleteScheduledAction = DeleteScheduledActionResponse

    request = post "DeleteScheduledAction"
    response _ _ = return (Right DeleteScheduledActionResponse)

data DeleteScheduledActionResponse = DeleteScheduledActionResponse
    deriving (Eq, Show, Generic)
