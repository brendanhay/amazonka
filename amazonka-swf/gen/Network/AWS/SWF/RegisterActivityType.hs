{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    , ratrqDefaultTaskScheduleToStartTimeout
    , ratrqDefaultTaskList
    , ratrqDefaultTaskPriority
    , ratrqDefaultTaskHeartbeatTimeout
    , ratrqDefaultTaskScheduleToCloseTimeout
    , ratrqDefaultTaskStartToCloseTimeout
    , ratrqDescription
    , ratrqDomain
    , ratrqName
    , ratrqVersion

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
-- * 'ratrqDefaultTaskScheduleToStartTimeout'
--
-- * 'ratrqDefaultTaskList'
--
-- * 'ratrqDefaultTaskPriority'
--
-- * 'ratrqDefaultTaskHeartbeatTimeout'
--
-- * 'ratrqDefaultTaskScheduleToCloseTimeout'
--
-- * 'ratrqDefaultTaskStartToCloseTimeout'
--
-- * 'ratrqDescription'
--
-- * 'ratrqDomain'
--
-- * 'ratrqName'
--
-- * 'ratrqVersion'
data RegisterActivityType = RegisterActivityType'
    { _ratrqDefaultTaskScheduleToStartTimeout :: !(Maybe Text)
    , _ratrqDefaultTaskList                   :: !(Maybe TaskList)
    , _ratrqDefaultTaskPriority               :: !(Maybe Text)
    , _ratrqDefaultTaskHeartbeatTimeout       :: !(Maybe Text)
    , _ratrqDefaultTaskScheduleToCloseTimeout :: !(Maybe Text)
    , _ratrqDefaultTaskStartToCloseTimeout    :: !(Maybe Text)
    , _ratrqDescription                       :: !(Maybe Text)
    , _ratrqDomain                            :: !Text
    , _ratrqName                              :: !Text
    , _ratrqVersion                           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterActivityType' smart constructor.
registerActivityType :: Text -> Text -> Text -> RegisterActivityType
registerActivityType pDomain pName pVersion =
    RegisterActivityType'
    { _ratrqDefaultTaskScheduleToStartTimeout = Nothing
    , _ratrqDefaultTaskList = Nothing
    , _ratrqDefaultTaskPriority = Nothing
    , _ratrqDefaultTaskHeartbeatTimeout = Nothing
    , _ratrqDefaultTaskScheduleToCloseTimeout = Nothing
    , _ratrqDefaultTaskStartToCloseTimeout = Nothing
    , _ratrqDescription = Nothing
    , _ratrqDomain = pDomain
    , _ratrqName = pName
    , _ratrqVersion = pVersion
    }

-- | If set, specifies the default maximum duration that a task of this
-- activity type can wait before being assigned to a worker. This default
-- can be overridden when scheduling an activity task using the
-- @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
ratrqDefaultTaskScheduleToStartTimeout :: Lens' RegisterActivityType (Maybe Text)
ratrqDefaultTaskScheduleToStartTimeout = lens _ratrqDefaultTaskScheduleToStartTimeout (\ s a -> s{_ratrqDefaultTaskScheduleToStartTimeout = a});

-- | If set, specifies the default task list to use for scheduling tasks of
-- this activity type. This default task list is used if a task list is not
-- provided when a task is scheduled through the @ScheduleActivityTask@
-- Decision.
ratrqDefaultTaskList :: Lens' RegisterActivityType (Maybe TaskList)
ratrqDefaultTaskList = lens _ratrqDefaultTaskList (\ s a -> s{_ratrqDefaultTaskList = a});

-- | The default task priority to assign to the activity type. If not
-- assigned, then \"0\" will be used. Valid values are integers that range
-- from Java\'s @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@
-- (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon Simple Workflow Developer Guide/.
ratrqDefaultTaskPriority :: Lens' RegisterActivityType (Maybe Text)
ratrqDefaultTaskPriority = lens _ratrqDefaultTaskPriority (\ s a -> s{_ratrqDefaultTaskPriority = a});

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
ratrqDefaultTaskHeartbeatTimeout :: Lens' RegisterActivityType (Maybe Text)
ratrqDefaultTaskHeartbeatTimeout = lens _ratrqDefaultTaskHeartbeatTimeout (\ s a -> s{_ratrqDefaultTaskHeartbeatTimeout = a});

-- | If set, specifies the default maximum duration for a task of this
-- activity type. This default can be overridden when scheduling an
-- activity task using the @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
ratrqDefaultTaskScheduleToCloseTimeout :: Lens' RegisterActivityType (Maybe Text)
ratrqDefaultTaskScheduleToCloseTimeout = lens _ratrqDefaultTaskScheduleToCloseTimeout (\ s a -> s{_ratrqDefaultTaskScheduleToCloseTimeout = a});

-- | If set, specifies the default maximum duration that a worker can take to
-- process tasks of this activity type. This default can be overridden when
-- scheduling an activity task using the @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
ratrqDefaultTaskStartToCloseTimeout :: Lens' RegisterActivityType (Maybe Text)
ratrqDefaultTaskStartToCloseTimeout = lens _ratrqDefaultTaskStartToCloseTimeout (\ s a -> s{_ratrqDefaultTaskStartToCloseTimeout = a});

-- | A textual description of the activity type.
ratrqDescription :: Lens' RegisterActivityType (Maybe Text)
ratrqDescription = lens _ratrqDescription (\ s a -> s{_ratrqDescription = a});

-- | The name of the domain in which this activity is to be registered.
ratrqDomain :: Lens' RegisterActivityType Text
ratrqDomain = lens _ratrqDomain (\ s a -> s{_ratrqDomain = a});

-- | The name of the activity type within the domain.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (\\u0000-\\u001f | \\u007f - \\u009f). Also, it must not
-- contain the literal string quotarnquot.
ratrqName :: Lens' RegisterActivityType Text
ratrqName = lens _ratrqName (\ s a -> s{_ratrqName = a});

-- | The version of the activity type.
--
-- The activity type consists of the name and version, the combination of
-- which must be unique within the domain.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (\\u0000-\\u001f | \\u007f - \\u009f). Also, it must not
-- contain the literal string quotarnquot.
ratrqVersion :: Lens' RegisterActivityType Text
ratrqVersion = lens _ratrqVersion (\ s a -> s{_ratrqVersion = a});

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
                 _ratrqDefaultTaskScheduleToStartTimeout,
               "defaultTaskList" .= _ratrqDefaultTaskList,
               "defaultTaskPriority" .= _ratrqDefaultTaskPriority,
               "defaultTaskHeartbeatTimeout" .=
                 _ratrqDefaultTaskHeartbeatTimeout,
               "defaultTaskScheduleToCloseTimeout" .=
                 _ratrqDefaultTaskScheduleToCloseTimeout,
               "defaultTaskStartToCloseTimeout" .=
                 _ratrqDefaultTaskStartToCloseTimeout,
               "description" .= _ratrqDescription,
               "domain" .= _ratrqDomain, "name" .= _ratrqName,
               "version" .= _ratrqVersion]

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
