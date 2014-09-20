{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DeleteScheduledAction
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
module Network.AWS.AutoScaling.DeleteScheduledAction
    (
    -- * Request
      DeleteScheduledAction
    -- ** Request constructor
    , deleteScheduledAction
    -- ** Request lenses
    , dsaAutoScalingGroupName
    , dsaScheduledActionName

    -- * Response
    , DeleteScheduledActionResponse
    -- ** Response constructor
    , deleteScheduledActionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

-- | 
data DeleteScheduledAction = DeleteScheduledAction
    { _dsaAutoScalingGroupName :: Maybe Text
    , _dsaScheduledActionName :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteScheduledAction' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingGroupName ::@ @Maybe Text@
--
-- * @ScheduledActionName ::@ @Text@
--
deleteScheduledAction :: Text -- ^ 'dsaScheduledActionName'
                      -> DeleteScheduledAction
deleteScheduledAction p2 = DeleteScheduledAction
    { _dsaAutoScalingGroupName = Nothing
    , _dsaScheduledActionName = p2
    }

-- | The name of the Auto Scaling group.
dsaAutoScalingGroupName :: Lens' DeleteScheduledAction (Maybe Text)
dsaAutoScalingGroupName =
    lens _dsaAutoScalingGroupName
         (\s a -> s { _dsaAutoScalingGroupName = a })

-- | The name of the action you want to delete.
dsaScheduledActionName :: Lens' DeleteScheduledAction Text
dsaScheduledActionName =
    lens _dsaScheduledActionName (\s a -> s { _dsaScheduledActionName = a })

instance ToQuery DeleteScheduledAction where
    toQuery = genericQuery def

data DeleteScheduledActionResponse = DeleteScheduledActionResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteScheduledActionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteScheduledActionResponse :: DeleteScheduledActionResponse
deleteScheduledActionResponse = DeleteScheduledActionResponse

instance AWSRequest DeleteScheduledAction where
    type Sv DeleteScheduledAction = AutoScaling
    type Rs DeleteScheduledAction = DeleteScheduledActionResponse

    request = post "DeleteScheduledAction"
    response _ = nullaryResponse DeleteScheduledActionResponse
