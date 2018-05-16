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
-- Module      : Network.AWS.SWF.RegisterWorkflowType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new /workflow type/ and its configuration settings in the specified domain.
--
--
-- The retention period for the workflow history is set by the 'RegisterDomain' action.
--
-- /Important:/ If the type already exists, then a @TypeAlreadyExists@ fault is returned. You cannot change the configuration settings of a workflow type once it is registered and it must be registered as a new version.
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
module Network.AWS.SWF.RegisterWorkflowType
    (
    -- * Creating a Request
      registerWorkflowType
    , RegisterWorkflowType
    -- * Request Lenses
    , rwtDefaultLambdaRole
    , rwtDefaultChildPolicy
    , rwtDefaultTaskList
    , rwtDefaultTaskPriority
    , rwtDefaultExecutionStartToCloseTimeout
    , rwtDefaultTaskStartToCloseTimeout
    , rwtDescription
    , rwtDomain
    , rwtName
    , rwtVersion

    -- * Destructuring the Response
    , registerWorkflowTypeResponse
    , RegisterWorkflowTypeResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types
import Network.AWS.SWF.Types.Product

-- | /See:/ 'registerWorkflowType' smart constructor.
data RegisterWorkflowType = RegisterWorkflowType'
  { _rwtDefaultLambdaRole                   :: !(Maybe Text)
  , _rwtDefaultChildPolicy                  :: !(Maybe ChildPolicy)
  , _rwtDefaultTaskList                     :: !(Maybe TaskList)
  , _rwtDefaultTaskPriority                 :: !(Maybe Text)
  , _rwtDefaultExecutionStartToCloseTimeout :: !(Maybe Text)
  , _rwtDefaultTaskStartToCloseTimeout      :: !(Maybe Text)
  , _rwtDescription                         :: !(Maybe Text)
  , _rwtDomain                              :: !Text
  , _rwtName                                :: !Text
  , _rwtVersion                             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterWorkflowType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rwtDefaultLambdaRole' - The default IAM role attached to this workflow type.
--
-- * 'rwtDefaultChildPolicy' - If set, specifies the default policy to use for the child workflow executions when a workflow execution of this type is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' . The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
--
-- * 'rwtDefaultTaskList' - If set, specifies the default task list to use for scheduling decision tasks for executions of this workflow type. This default is used only if a task list isn't provided when starting the execution through the 'StartWorkflowExecution' Action or @StartChildWorkflowExecution@ 'Decision' .
--
-- * 'rwtDefaultTaskPriority' - The default task priority to assign to the workflow type. If not assigned, then @0@ is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- * 'rwtDefaultExecutionStartToCloseTimeout' - If set, specifies the default maximum duration for executions of this workflow type. You can override this default when starting an execution through the 'StartWorkflowExecution' Action or @StartChildWorkflowExecution@ 'Decision' . The duration is specified in seconds; an integer greater than or equal to 0. Unlike some of the other timeout parameters in Amazon SWF, you cannot specify a value of "NONE" for @defaultExecutionStartToCloseTimeout@ ; there is a one-year max limit on the time that a workflow execution can run. Exceeding this limit always causes the workflow execution to time out.
--
-- * 'rwtDefaultTaskStartToCloseTimeout' - If set, specifies the default maximum duration of decision tasks for this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'rwtDescription' - Textual description of the workflow type.
--
-- * 'rwtDomain' - The name of the domain in which to register the workflow type.
--
-- * 'rwtName' - The name of the workflow type. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
--
-- * 'rwtVersion' - The version of the workflow type. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
registerWorkflowType
    :: Text -- ^ 'rwtDomain'
    -> Text -- ^ 'rwtName'
    -> Text -- ^ 'rwtVersion'
    -> RegisterWorkflowType
registerWorkflowType pDomain_ pName_ pVersion_ =
  RegisterWorkflowType'
    { _rwtDefaultLambdaRole = Nothing
    , _rwtDefaultChildPolicy = Nothing
    , _rwtDefaultTaskList = Nothing
    , _rwtDefaultTaskPriority = Nothing
    , _rwtDefaultExecutionStartToCloseTimeout = Nothing
    , _rwtDefaultTaskStartToCloseTimeout = Nothing
    , _rwtDescription = Nothing
    , _rwtDomain = pDomain_
    , _rwtName = pName_
    , _rwtVersion = pVersion_
    }


-- | The default IAM role attached to this workflow type.
rwtDefaultLambdaRole :: Lens' RegisterWorkflowType (Maybe Text)
rwtDefaultLambdaRole = lens _rwtDefaultLambdaRole (\ s a -> s{_rwtDefaultLambdaRole = a})

-- | If set, specifies the default policy to use for the child workflow executions when a workflow execution of this type is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' . The supported child policies are:     * @TERMINATE@ – The child executions are terminated.     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.     * @ABANDON@ – No action is taken. The child executions continue to run.
rwtDefaultChildPolicy :: Lens' RegisterWorkflowType (Maybe ChildPolicy)
rwtDefaultChildPolicy = lens _rwtDefaultChildPolicy (\ s a -> s{_rwtDefaultChildPolicy = a})

-- | If set, specifies the default task list to use for scheduling decision tasks for executions of this workflow type. This default is used only if a task list isn't provided when starting the execution through the 'StartWorkflowExecution' Action or @StartChildWorkflowExecution@ 'Decision' .
rwtDefaultTaskList :: Lens' RegisterWorkflowType (Maybe TaskList)
rwtDefaultTaskList = lens _rwtDefaultTaskList (\ s a -> s{_rwtDefaultTaskList = a})

-- | The default task priority to assign to the workflow type. If not assigned, then @0@ is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
rwtDefaultTaskPriority :: Lens' RegisterWorkflowType (Maybe Text)
rwtDefaultTaskPriority = lens _rwtDefaultTaskPriority (\ s a -> s{_rwtDefaultTaskPriority = a})

-- | If set, specifies the default maximum duration for executions of this workflow type. You can override this default when starting an execution through the 'StartWorkflowExecution' Action or @StartChildWorkflowExecution@ 'Decision' . The duration is specified in seconds; an integer greater than or equal to 0. Unlike some of the other timeout parameters in Amazon SWF, you cannot specify a value of "NONE" for @defaultExecutionStartToCloseTimeout@ ; there is a one-year max limit on the time that a workflow execution can run. Exceeding this limit always causes the workflow execution to time out.
rwtDefaultExecutionStartToCloseTimeout :: Lens' RegisterWorkflowType (Maybe Text)
rwtDefaultExecutionStartToCloseTimeout = lens _rwtDefaultExecutionStartToCloseTimeout (\ s a -> s{_rwtDefaultExecutionStartToCloseTimeout = a})

-- | If set, specifies the default maximum duration of decision tasks for this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
rwtDefaultTaskStartToCloseTimeout :: Lens' RegisterWorkflowType (Maybe Text)
rwtDefaultTaskStartToCloseTimeout = lens _rwtDefaultTaskStartToCloseTimeout (\ s a -> s{_rwtDefaultTaskStartToCloseTimeout = a})

-- | Textual description of the workflow type.
rwtDescription :: Lens' RegisterWorkflowType (Maybe Text)
rwtDescription = lens _rwtDescription (\ s a -> s{_rwtDescription = a})

-- | The name of the domain in which to register the workflow type.
rwtDomain :: Lens' RegisterWorkflowType Text
rwtDomain = lens _rwtDomain (\ s a -> s{_rwtDomain = a})

-- | The name of the workflow type. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
rwtName :: Lens' RegisterWorkflowType Text
rwtName = lens _rwtName (\ s a -> s{_rwtName = a})

-- | The version of the workflow type. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
rwtVersion :: Lens' RegisterWorkflowType Text
rwtVersion = lens _rwtVersion (\ s a -> s{_rwtVersion = a})

instance AWSRequest RegisterWorkflowType where
        type Rs RegisterWorkflowType =
             RegisterWorkflowTypeResponse
        request = postJSON swf
        response = receiveNull RegisterWorkflowTypeResponse'

instance Hashable RegisterWorkflowType where

instance NFData RegisterWorkflowType where

instance ToHeaders RegisterWorkflowType where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.RegisterWorkflowType" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON RegisterWorkflowType where
        toJSON RegisterWorkflowType'{..}
          = object
              (catMaybes
                 [("defaultLambdaRole" .=) <$> _rwtDefaultLambdaRole,
                  ("defaultChildPolicy" .=) <$> _rwtDefaultChildPolicy,
                  ("defaultTaskList" .=) <$> _rwtDefaultTaskList,
                  ("defaultTaskPriority" .=) <$>
                    _rwtDefaultTaskPriority,
                  ("defaultExecutionStartToCloseTimeout" .=) <$>
                    _rwtDefaultExecutionStartToCloseTimeout,
                  ("defaultTaskStartToCloseTimeout" .=) <$>
                    _rwtDefaultTaskStartToCloseTimeout,
                  ("description" .=) <$> _rwtDescription,
                  Just ("domain" .= _rwtDomain),
                  Just ("name" .= _rwtName),
                  Just ("version" .= _rwtVersion)])

instance ToPath RegisterWorkflowType where
        toPath = const "/"

instance ToQuery RegisterWorkflowType where
        toQuery = const mempty

-- | /See:/ 'registerWorkflowTypeResponse' smart constructor.
data RegisterWorkflowTypeResponse =
  RegisterWorkflowTypeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterWorkflowTypeResponse' with the minimum fields required to make a request.
--
registerWorkflowTypeResponse
    :: RegisterWorkflowTypeResponse
registerWorkflowTypeResponse = RegisterWorkflowTypeResponse'


instance NFData RegisterWorkflowTypeResponse where
