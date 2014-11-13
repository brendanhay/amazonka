{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.SWF.RegisterActivityType
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Registers a new activity type along with its configuration settings in the
-- specified domain. A TypeAlreadyExists fault is returned if the type already
-- exists in the domain. You cannot change any configuration settings of the
-- type after its registration, and it must be registered as a new version.
-- Access Control You can use IAM policies to control this action's access to
-- Amazon SWF resources as follows: Use a Resource element with the domain
-- name to limit the action to only specified domains. Use an Action element
-- to allow or deny permission to call this action. Constrain the following
-- parameters by using a Condition element with the appropriate keys.
-- defaultTaskList.name: String constraint. The key is
-- swf:defaultTaskList.name. name: String constraint. The key is swf:name.
-- version: String constraint. The key is swf:version. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
module Network.AWS.SWF.RegisterActivityType
    (
    -- * Request
      RegisterActivityType
    -- ** Request constructor
    , registerActivityType
    -- ** Request lenses
    , ratDefaultTaskHeartbeatTimeout
    , ratDefaultTaskList
    , ratDefaultTaskScheduleToCloseTimeout
    , ratDefaultTaskScheduleToStartTimeout
    , ratDefaultTaskStartToCloseTimeout
    , ratDescription
    , ratDomain
    , ratName
    , ratVersion

    -- * Response
    , RegisterActivityTypeResponse
    -- ** Response constructor
    , registerActivityTypeResponse
    ) where

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.SWF.Types

data RegisterActivityType = RegisterActivityType
    { _ratDefaultTaskHeartbeatTimeout       :: Maybe Text
    , _ratDefaultTaskList                   :: Maybe TaskList
    , _ratDefaultTaskScheduleToCloseTimeout :: Maybe Text
    , _ratDefaultTaskScheduleToStartTimeout :: Maybe Text
    , _ratDefaultTaskStartToCloseTimeout    :: Maybe Text
    , _ratDescription                       :: Maybe Text
    , _ratDomain                            :: Text
    , _ratName                              :: Text
    , _ratVersion                           :: Text
    } deriving (Eq, Show, Generic)

-- | 'RegisterActivityType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ratDefaultTaskHeartbeatTimeout' @::@ 'Maybe' 'Text'
--
-- * 'ratDefaultTaskList' @::@ 'Maybe' 'TaskList'
--
-- * 'ratDefaultTaskScheduleToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'ratDefaultTaskScheduleToStartTimeout' @::@ 'Maybe' 'Text'
--
-- * 'ratDefaultTaskStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'ratDescription' @::@ 'Maybe' 'Text'
--
-- * 'ratDomain' @::@ 'Text'
--
-- * 'ratName' @::@ 'Text'
--
-- * 'ratVersion' @::@ 'Text'
--
registerActivityType :: Text -- ^ 'ratDomain'
                     -> Text -- ^ 'ratName'
                     -> Text -- ^ 'ratVersion'
                     -> RegisterActivityType
registerActivityType p1 p2 p3 = RegisterActivityType
    { _ratDomain                            = p1
    , _ratName                              = p2
    , _ratVersion                           = p3
    , _ratDescription                       = Nothing
    , _ratDefaultTaskStartToCloseTimeout    = Nothing
    , _ratDefaultTaskHeartbeatTimeout       = Nothing
    , _ratDefaultTaskList                   = Nothing
    , _ratDefaultTaskScheduleToStartTimeout = Nothing
    , _ratDefaultTaskScheduleToCloseTimeout = Nothing
    }

-- | If set, specifies the default maximum time before which a worker
-- processing a task of this type must report progress by calling
-- RecordActivityTaskHeartbeat. If the timeout is exceeded, the activity
-- task is automatically timed out. This default can be overridden when
-- scheduling an activity task using the ScheduleActivityTask Decision. If
-- the activity worker subsequently attempts to record a heartbeat or
-- returns a result, the activity worker receives an UnknownResource fault.
-- In this case, Amazon SWF no longer considers the activity task to be
-- valid; the activity worker should clean up the activity task. The valid
-- values are integers greater than or equal to 0. An integer value can be
-- used to specify the duration in seconds while NONE can be used to specify
-- unlimited duration.
ratDefaultTaskHeartbeatTimeout :: Lens' RegisterActivityType (Maybe Text)
ratDefaultTaskHeartbeatTimeout =
    lens _ratDefaultTaskHeartbeatTimeout
        (\s a -> s { _ratDefaultTaskHeartbeatTimeout = a })

-- | If set, specifies the default task list to use for scheduling tasks of
-- this activity type. This default task list is used if a task list is not
-- provided when a task is scheduled through the ScheduleActivityTask
-- Decision.
ratDefaultTaskList :: Lens' RegisterActivityType (Maybe TaskList)
ratDefaultTaskList =
    lens _ratDefaultTaskList (\s a -> s { _ratDefaultTaskList = a })

-- | If set, specifies the default maximum duration for a task of this
-- activity type. This default can be overridden when scheduling an activity
-- task using the ScheduleActivityTask Decision. The valid values are
-- integers greater than or equal to 0. An integer value can be used to
-- specify the duration in seconds while NONE can be used to specify
-- unlimited duration.
ratDefaultTaskScheduleToCloseTimeout :: Lens' RegisterActivityType (Maybe Text)
ratDefaultTaskScheduleToCloseTimeout =
    lens _ratDefaultTaskScheduleToCloseTimeout
        (\s a -> s { _ratDefaultTaskScheduleToCloseTimeout = a })

-- | If set, specifies the default maximum duration that a task of this
-- activity type can wait before being assigned to a worker. This default
-- can be overridden when scheduling an activity task using the
-- ScheduleActivityTask Decision. The valid values are integers greater than
-- or equal to 0. An integer value can be used to specify the duration in
-- seconds while NONE can be used to specify unlimited duration.
ratDefaultTaskScheduleToStartTimeout :: Lens' RegisterActivityType (Maybe Text)
ratDefaultTaskScheduleToStartTimeout =
    lens _ratDefaultTaskScheduleToStartTimeout
        (\s a -> s { _ratDefaultTaskScheduleToStartTimeout = a })

-- | If set, specifies the default maximum duration that a worker can take to
-- process tasks of this activity type. This default can be overridden when
-- scheduling an activity task using the ScheduleActivityTask Decision. The
-- valid values are integers greater than or equal to 0. An integer value
-- can be used to specify the duration in seconds while NONE can be used to
-- specify unlimited duration.
ratDefaultTaskStartToCloseTimeout :: Lens' RegisterActivityType (Maybe Text)
ratDefaultTaskStartToCloseTimeout =
    lens _ratDefaultTaskStartToCloseTimeout
        (\s a -> s { _ratDefaultTaskStartToCloseTimeout = a })

-- | A textual description of the activity type.
ratDescription :: Lens' RegisterActivityType (Maybe Text)
ratDescription = lens _ratDescription (\s a -> s { _ratDescription = a })

-- | The name of the domain in which this activity is to be registered.
ratDomain :: Lens' RegisterActivityType Text
ratDomain = lens _ratDomain (\s a -> s { _ratDomain = a })

-- | The name of the activity type within the domain. The specified string
-- must not start or end with whitespace. It must not contain a : (colon), /
-- (slash), | (vertical bar), or any control characters (\u0000-\u001f |
-- \u007f - \u009f). Also, it must not contain the literal string
-- &quot;arn&quot;.
ratName :: Lens' RegisterActivityType Text
ratName = lens _ratName (\s a -> s { _ratName = a })

-- | The version of the activity type. The specified string must not start or
-- end with whitespace. It must not contain a : (colon), / (slash), |
-- (vertical bar), or any control characters (\u0000-\u001f | \u007f -
-- \u009f). Also, it must not contain the literal string &quot;arn&quot;.
ratVersion :: Lens' RegisterActivityType Text
ratVersion = lens _ratVersion (\s a -> s { _ratVersion = a })

instance ToPath RegisterActivityType where
    toPath = const "/"

instance ToQuery RegisterActivityType where
    toQuery = const mempty

instance ToHeaders RegisterActivityType

instance ToBody RegisterActivityType where
    toBody = toBody . encode . _ratDomain

data RegisterActivityTypeResponse = RegisterActivityTypeResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RegisterActivityTypeResponse' constructor.
registerActivityTypeResponse :: RegisterActivityTypeResponse
registerActivityTypeResponse = RegisterActivityTypeResponse

-- FromJSON

instance AWSRequest RegisterActivityType where
    type Sv RegisterActivityType = SWF
    type Rs RegisterActivityType = RegisterActivityTypeResponse

    request  = post'
    response = nullaryResponse RegisterActivityTypeResponse
