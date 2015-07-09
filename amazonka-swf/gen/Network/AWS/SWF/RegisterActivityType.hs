{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.RegisterActivityType
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Registers a new /activity type/ along with its configuration settings in
-- the specified domain.
--
-- A @TypeAlreadyExists@ fault is returned if the type already exists in
-- the domain. You cannot change any configuration settings of the type
-- after its registration, and it must be registered as a new version.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   Constrain the following parameters by using a @Condition@ element
--     with the appropriate keys.
--     -   @defaultTaskList.name@: String constraint. The key is
--         @swf:defaultTaskList.name@.
--     -   @name@: String constraint. The key is @swf:name@.
--     -   @version@: String constraint. The key is @swf:version@.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_RegisterActivityType.html>
module Network.AWS.SWF.RegisterActivityType
    (
    -- * Request
      RegisterActivityType
    -- ** Request constructor
    , registerActivityType
    -- ** Request lenses
    , ratDefaultTaskScheduleToStartTimeout
    , ratDefaultTaskList
    , ratDefaultTaskPriority
    , ratDefaultTaskHeartbeatTimeout
    , ratDefaultTaskScheduleToCloseTimeout
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

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'registerActivityType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ratDefaultTaskScheduleToStartTimeout'
--
-- * 'ratDefaultTaskList'
--
-- * 'ratDefaultTaskPriority'
--
-- * 'ratDefaultTaskHeartbeatTimeout'
--
-- * 'ratDefaultTaskScheduleToCloseTimeout'
--
-- * 'ratDefaultTaskStartToCloseTimeout'
--
-- * 'ratDescription'
--
-- * 'ratDomain'
--
-- * 'ratName'
--
-- * 'ratVersion'
data RegisterActivityType = RegisterActivityType'
    { _ratDefaultTaskScheduleToStartTimeout :: !(Maybe Text)
    , _ratDefaultTaskList                   :: !(Maybe TaskList)
    , _ratDefaultTaskPriority               :: !(Maybe Text)
    , _ratDefaultTaskHeartbeatTimeout       :: !(Maybe Text)
    , _ratDefaultTaskScheduleToCloseTimeout :: !(Maybe Text)
    , _ratDefaultTaskStartToCloseTimeout    :: !(Maybe Text)
    , _ratDescription                       :: !(Maybe Text)
    , _ratDomain                            :: !Text
    , _ratName                              :: !Text
    , _ratVersion                           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterActivityType' smart constructor.
registerActivityType :: Text -> Text -> Text -> RegisterActivityType
registerActivityType pDomain pName pVersion =
    RegisterActivityType'
    { _ratDefaultTaskScheduleToStartTimeout = Nothing
    , _ratDefaultTaskList = Nothing
    , _ratDefaultTaskPriority = Nothing
    , _ratDefaultTaskHeartbeatTimeout = Nothing
    , _ratDefaultTaskScheduleToCloseTimeout = Nothing
    , _ratDefaultTaskStartToCloseTimeout = Nothing
    , _ratDescription = Nothing
    , _ratDomain = pDomain
    , _ratName = pName
    , _ratVersion = pVersion
    }

-- | If set, specifies the default maximum duration that a task of this
-- activity type can wait before being assigned to a worker. This default
-- can be overridden when scheduling an activity task using the
-- @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
ratDefaultTaskScheduleToStartTimeout :: Lens' RegisterActivityType (Maybe Text)
ratDefaultTaskScheduleToStartTimeout = lens _ratDefaultTaskScheduleToStartTimeout (\ s a -> s{_ratDefaultTaskScheduleToStartTimeout = a});

-- | If set, specifies the default task list to use for scheduling tasks of
-- this activity type. This default task list is used if a task list is not
-- provided when a task is scheduled through the @ScheduleActivityTask@
-- Decision.
ratDefaultTaskList :: Lens' RegisterActivityType (Maybe TaskList)
ratDefaultTaskList = lens _ratDefaultTaskList (\ s a -> s{_ratDefaultTaskList = a});

-- | The default task priority to assign to the activity type. If not
-- assigned, then \"0\" will be used. Valid values are integers that range
-- from Java\'s @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@
-- (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon Simple Workflow Developer Guide/.
ratDefaultTaskPriority :: Lens' RegisterActivityType (Maybe Text)
ratDefaultTaskPriority = lens _ratDefaultTaskPriority (\ s a -> s{_ratDefaultTaskPriority = a});

-- | If set, specifies the default maximum time before which a worker
-- processing a task of this type must report progress by calling
-- RecordActivityTaskHeartbeat. If the timeout is exceeded, the activity
-- task is automatically timed out. This default can be overridden when
-- scheduling an activity task using the @ScheduleActivityTask@ Decision.
-- If the activity worker subsequently attempts to record a heartbeat or
-- returns a result, the activity worker receives an @UnknownResource@
-- fault. In this case, Amazon SWF no longer considers the activity task to
-- be valid; the activity worker should clean up the activity task.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
ratDefaultTaskHeartbeatTimeout :: Lens' RegisterActivityType (Maybe Text)
ratDefaultTaskHeartbeatTimeout = lens _ratDefaultTaskHeartbeatTimeout (\ s a -> s{_ratDefaultTaskHeartbeatTimeout = a});

-- | If set, specifies the default maximum duration for a task of this
-- activity type. This default can be overridden when scheduling an
-- activity task using the @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
ratDefaultTaskScheduleToCloseTimeout :: Lens' RegisterActivityType (Maybe Text)
ratDefaultTaskScheduleToCloseTimeout = lens _ratDefaultTaskScheduleToCloseTimeout (\ s a -> s{_ratDefaultTaskScheduleToCloseTimeout = a});

-- | If set, specifies the default maximum duration that a worker can take to
-- process tasks of this activity type. This default can be overridden when
-- scheduling an activity task using the @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
ratDefaultTaskStartToCloseTimeout :: Lens' RegisterActivityType (Maybe Text)
ratDefaultTaskStartToCloseTimeout = lens _ratDefaultTaskStartToCloseTimeout (\ s a -> s{_ratDefaultTaskStartToCloseTimeout = a});

-- | A textual description of the activity type.
ratDescription :: Lens' RegisterActivityType (Maybe Text)
ratDescription = lens _ratDescription (\ s a -> s{_ratDescription = a});

-- | The name of the domain in which this activity is to be registered.
ratDomain :: Lens' RegisterActivityType Text
ratDomain = lens _ratDomain (\ s a -> s{_ratDomain = a});

-- | The name of the activity type within the domain.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (\\u0000-\\u001f | \\u007f - \\u009f). Also, it must not
-- contain the literal string quotarnquot.
ratName :: Lens' RegisterActivityType Text
ratName = lens _ratName (\ s a -> s{_ratName = a});

-- | The version of the activity type.
--
-- The activity type consists of the name and version, the combination of
-- which must be unique within the domain.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (\\u0000-\\u001f | \\u007f - \\u009f). Also, it must not
-- contain the literal string quotarnquot.
ratVersion :: Lens' RegisterActivityType Text
ratVersion = lens _ratVersion (\ s a -> s{_ratVersion = a});

instance AWSRequest RegisterActivityType where
        type Sv RegisterActivityType = SWF
        type Rs RegisterActivityType =
             RegisterActivityTypeResponse
        request = postJSON
        response = receiveNull RegisterActivityTypeResponse'

instance ToHeaders RegisterActivityType where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.RegisterActivityType" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON RegisterActivityType where
        toJSON RegisterActivityType'{..}
          = object
              ["defaultTaskScheduleToStartTimeout" .=
                 _ratDefaultTaskScheduleToStartTimeout,
               "defaultTaskList" .= _ratDefaultTaskList,
               "defaultTaskPriority" .= _ratDefaultTaskPriority,
               "defaultTaskHeartbeatTimeout" .=
                 _ratDefaultTaskHeartbeatTimeout,
               "defaultTaskScheduleToCloseTimeout" .=
                 _ratDefaultTaskScheduleToCloseTimeout,
               "defaultTaskStartToCloseTimeout" .=
                 _ratDefaultTaskStartToCloseTimeout,
               "description" .= _ratDescription,
               "domain" .= _ratDomain, "name" .= _ratName,
               "version" .= _ratVersion]

instance ToPath RegisterActivityType where
        toPath = const "/"

instance ToQuery RegisterActivityType where
        toQuery = const mempty

-- | /See:/ 'registerActivityTypeResponse' smart constructor.
data RegisterActivityTypeResponse =
    RegisterActivityTypeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterActivityTypeResponse' smart constructor.
registerActivityTypeResponse :: RegisterActivityTypeResponse
registerActivityTypeResponse = RegisterActivityTypeResponse'
