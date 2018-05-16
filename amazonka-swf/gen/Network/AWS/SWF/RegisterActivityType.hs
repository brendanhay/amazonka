{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.RegisterActivityType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new /activity type/ along with its configuration settings in the specified domain.
--
--
-- /Important:/ A @TypeAlreadyExists@ fault is returned if the type already exists in the domain. You cannot change any configuration settings of the type after its registration, and it must be registered as a new version.
--
-- __Access Control__
--
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @defaultTaskList.name@ : String constraint. The key is @swf:defaultTaskList.name@ .
--
--     * @name@ : String constraint. The key is @swf:name@ .
--
--     * @version@ : String constraint. The key is @swf:version@ .
--
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
module Network.AWS.SWF.RegisterActivityType
    (
    -- * Creating a Request
      registerActivityType
    , RegisterActivityType
    -- * Request Lenses
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

    -- * Destructuring the Response
    , registerActivityTypeResponse
    , RegisterActivityTypeResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types
import Network.AWS.SWF.Types.Product

-- | /See:/ 'registerActivityType' smart constructor.
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterActivityType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ratDefaultTaskScheduleToStartTimeout' - If set, specifies the default maximum duration that a task of this activity type can wait before being assigned to a worker. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'ratDefaultTaskList' - If set, specifies the default task list to use for scheduling tasks of this activity type. This default task list is used if a task list isn't provided when a task is scheduled through the @ScheduleActivityTask@ 'Decision' .
--
-- * 'ratDefaultTaskPriority' - The default task priority to assign to the activity type. If not assigned, then @0@ is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /in the \/Amazon SWF Developer Guide\/ ./ .
--
-- * 'ratDefaultTaskHeartbeatTimeout' - If set, specifies the default maximum time before which a worker processing a task of this type must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' . If the activity worker subsequently attempts to record a heartbeat or returns a result, the activity worker receives an @UnknownResource@ fault. In this case, Amazon SWF no longer considers the activity task to be valid; the activity worker should clean up the activity task. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'ratDefaultTaskScheduleToCloseTimeout' - If set, specifies the default maximum duration for a task of this activity type. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'ratDefaultTaskStartToCloseTimeout' - If set, specifies the default maximum duration that a worker can take to process tasks of this activity type. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'ratDescription' - A textual description of the activity type.
--
-- * 'ratDomain' - The name of the domain in which this activity is to be registered.
--
-- * 'ratName' - The name of the activity type within the domain. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
--
-- * 'ratVersion' - The version of the activity type. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
registerActivityType
    :: Text -- ^ 'ratDomain'
    -> Text -- ^ 'ratName'
    -> Text -- ^ 'ratVersion'
    -> RegisterActivityType
registerActivityType pDomain_ pName_ pVersion_ =
  RegisterActivityType'
    { _ratDefaultTaskScheduleToStartTimeout = Nothing
    , _ratDefaultTaskList = Nothing
    , _ratDefaultTaskPriority = Nothing
    , _ratDefaultTaskHeartbeatTimeout = Nothing
    , _ratDefaultTaskScheduleToCloseTimeout = Nothing
    , _ratDefaultTaskStartToCloseTimeout = Nothing
    , _ratDescription = Nothing
    , _ratDomain = pDomain_
    , _ratName = pName_
    , _ratVersion = pVersion_
    }


-- | If set, specifies the default maximum duration that a task of this activity type can wait before being assigned to a worker. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
ratDefaultTaskScheduleToStartTimeout :: Lens' RegisterActivityType (Maybe Text)
ratDefaultTaskScheduleToStartTimeout = lens _ratDefaultTaskScheduleToStartTimeout (\ s a -> s{_ratDefaultTaskScheduleToStartTimeout = a})

-- | If set, specifies the default task list to use for scheduling tasks of this activity type. This default task list is used if a task list isn't provided when a task is scheduled through the @ScheduleActivityTask@ 'Decision' .
ratDefaultTaskList :: Lens' RegisterActivityType (Maybe TaskList)
ratDefaultTaskList = lens _ratDefaultTaskList (\ s a -> s{_ratDefaultTaskList = a})

-- | The default task priority to assign to the activity type. If not assigned, then @0@ is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /in the \/Amazon SWF Developer Guide\/ ./ .
ratDefaultTaskPriority :: Lens' RegisterActivityType (Maybe Text)
ratDefaultTaskPriority = lens _ratDefaultTaskPriority (\ s a -> s{_ratDefaultTaskPriority = a})

-- | If set, specifies the default maximum time before which a worker processing a task of this type must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' . If the activity worker subsequently attempts to record a heartbeat or returns a result, the activity worker receives an @UnknownResource@ fault. In this case, Amazon SWF no longer considers the activity task to be valid; the activity worker should clean up the activity task. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
ratDefaultTaskHeartbeatTimeout :: Lens' RegisterActivityType (Maybe Text)
ratDefaultTaskHeartbeatTimeout = lens _ratDefaultTaskHeartbeatTimeout (\ s a -> s{_ratDefaultTaskHeartbeatTimeout = a})

-- | If set, specifies the default maximum duration for a task of this activity type. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
ratDefaultTaskScheduleToCloseTimeout :: Lens' RegisterActivityType (Maybe Text)
ratDefaultTaskScheduleToCloseTimeout = lens _ratDefaultTaskScheduleToCloseTimeout (\ s a -> s{_ratDefaultTaskScheduleToCloseTimeout = a})

-- | If set, specifies the default maximum duration that a worker can take to process tasks of this activity type. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
ratDefaultTaskStartToCloseTimeout :: Lens' RegisterActivityType (Maybe Text)
ratDefaultTaskStartToCloseTimeout = lens _ratDefaultTaskStartToCloseTimeout (\ s a -> s{_ratDefaultTaskStartToCloseTimeout = a})

-- | A textual description of the activity type.
ratDescription :: Lens' RegisterActivityType (Maybe Text)
ratDescription = lens _ratDescription (\ s a -> s{_ratDescription = a})

-- | The name of the domain in which this activity is to be registered.
ratDomain :: Lens' RegisterActivityType Text
ratDomain = lens _ratDomain (\ s a -> s{_ratDomain = a})

-- | The name of the activity type within the domain. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
ratName :: Lens' RegisterActivityType Text
ratName = lens _ratName (\ s a -> s{_ratName = a})

-- | The version of the activity type. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
ratVersion :: Lens' RegisterActivityType Text
ratVersion = lens _ratVersion (\ s a -> s{_ratVersion = a})

instance AWSRequest RegisterActivityType where
        type Rs RegisterActivityType =
             RegisterActivityTypeResponse
        request = postJSON swf
        response = receiveNull RegisterActivityTypeResponse'

instance Hashable RegisterActivityType where

instance NFData RegisterActivityType where

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
              (catMaybes
                 [("defaultTaskScheduleToStartTimeout" .=) <$>
                    _ratDefaultTaskScheduleToStartTimeout,
                  ("defaultTaskList" .=) <$> _ratDefaultTaskList,
                  ("defaultTaskPriority" .=) <$>
                    _ratDefaultTaskPriority,
                  ("defaultTaskHeartbeatTimeout" .=) <$>
                    _ratDefaultTaskHeartbeatTimeout,
                  ("defaultTaskScheduleToCloseTimeout" .=) <$>
                    _ratDefaultTaskScheduleToCloseTimeout,
                  ("defaultTaskStartToCloseTimeout" .=) <$>
                    _ratDefaultTaskStartToCloseTimeout,
                  ("description" .=) <$> _ratDescription,
                  Just ("domain" .= _ratDomain),
                  Just ("name" .= _ratName),
                  Just ("version" .= _ratVersion)])

instance ToPath RegisterActivityType where
        toPath = const "/"

instance ToQuery RegisterActivityType where
        toQuery = const mempty

-- | /See:/ 'registerActivityTypeResponse' smart constructor.
data RegisterActivityTypeResponse =
  RegisterActivityTypeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterActivityTypeResponse' with the minimum fields required to make a request.
--
registerActivityTypeResponse
    :: RegisterActivityTypeResponse
registerActivityTypeResponse = RegisterActivityTypeResponse'


instance NFData RegisterActivityTypeResponse where
