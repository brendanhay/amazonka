{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.RegisterWorkflowType
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Registers a new /workflow type/ and its configuration settings in the
-- specified domain.
--
-- The retention period for the workflow history is set by the
-- RegisterDomain action.
--
-- If the type already exists, then a @TypeAlreadyExists@ fault is
-- returned. You cannot change the configuration settings of a workflow
-- type once it is registered and it must be registered as a new version.
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
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_RegisterWorkflowType.html>
module Network.AWS.SWF.RegisterWorkflowType
    (
    -- * Request
      RegisterWorkflowType
    -- ** Request constructor
    , registerWorkflowType
    -- ** Request lenses
    , rwtrqDefaultChildPolicy
    , rwtrqDefaultTaskList
    , rwtrqDefaultTaskPriority
    , rwtrqDefaultExecutionStartToCloseTimeout
    , rwtrqDefaultTaskStartToCloseTimeout
    , rwtrqDescription
    , rwtrqDomain
    , rwtrqName
    , rwtrqVersion

    -- * Response
    , RegisterWorkflowTypeResponse
    -- ** Response constructor
    , registerWorkflowTypeResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'registerWorkflowType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rwtrqDefaultChildPolicy'
--
-- * 'rwtrqDefaultTaskList'
--
-- * 'rwtrqDefaultTaskPriority'
--
-- * 'rwtrqDefaultExecutionStartToCloseTimeout'
--
-- * 'rwtrqDefaultTaskStartToCloseTimeout'
--
-- * 'rwtrqDescription'
--
-- * 'rwtrqDomain'
--
-- * 'rwtrqName'
--
-- * 'rwtrqVersion'
data RegisterWorkflowType = RegisterWorkflowType'
    { _rwtrqDefaultChildPolicy                  :: !(Maybe ChildPolicy)
    , _rwtrqDefaultTaskList                     :: !(Maybe TaskList)
    , _rwtrqDefaultTaskPriority                 :: !(Maybe Text)
    , _rwtrqDefaultExecutionStartToCloseTimeout :: !(Maybe Text)
    , _rwtrqDefaultTaskStartToCloseTimeout      :: !(Maybe Text)
    , _rwtrqDescription                         :: !(Maybe Text)
    , _rwtrqDomain                              :: !Text
    , _rwtrqName                                :: !Text
    , _rwtrqVersion                             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterWorkflowType' smart constructor.
registerWorkflowType :: Text -> Text -> Text -> RegisterWorkflowType
registerWorkflowType pDomain_ pName_ pVersion_ =
    RegisterWorkflowType'
    { _rwtrqDefaultChildPolicy = Nothing
    , _rwtrqDefaultTaskList = Nothing
    , _rwtrqDefaultTaskPriority = Nothing
    , _rwtrqDefaultExecutionStartToCloseTimeout = Nothing
    , _rwtrqDefaultTaskStartToCloseTimeout = Nothing
    , _rwtrqDescription = Nothing
    , _rwtrqDomain = pDomain_
    , _rwtrqName = pName_
    , _rwtrqVersion = pVersion_
    }

-- | If set, specifies the default policy to use for the child workflow
-- executions when a workflow execution of this type is terminated, by
-- calling the TerminateWorkflowExecution action explicitly or due to an
-- expired timeout. This default can be overridden when starting a workflow
-- execution using the StartWorkflowExecution action or the
-- @StartChildWorkflowExecution@ Decision.
--
-- The supported child policies are:
--
-- -   __TERMINATE:__ the child executions will be terminated.
-- -   __REQUEST_CANCEL:__ a request to cancel will be attempted for each
--     child execution by recording a @WorkflowExecutionCancelRequested@
--     event in its history. It is up to the decider to take appropriate
--     actions when it receives an execution history with this event.
-- -   __ABANDON:__ no action will be taken. The child executions will
--     continue to run.
rwtrqDefaultChildPolicy :: Lens' RegisterWorkflowType (Maybe ChildPolicy)
rwtrqDefaultChildPolicy = lens _rwtrqDefaultChildPolicy (\ s a -> s{_rwtrqDefaultChildPolicy = a});

-- | If set, specifies the default task list to use for scheduling decision
-- tasks for executions of this workflow type. This default is used only if
-- a task list is not provided when starting the execution through the
-- StartWorkflowExecution Action or @StartChildWorkflowExecution@ Decision.
rwtrqDefaultTaskList :: Lens' RegisterWorkflowType (Maybe TaskList)
rwtrqDefaultTaskList = lens _rwtrqDefaultTaskList (\ s a -> s{_rwtrqDefaultTaskList = a});

-- | The default task priority to assign to the workflow type. If not
-- assigned, then \"0\" will be used. Valid values are integers that range
-- from Java\'s @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@
-- (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon Simple Workflow Developer Guide/.
rwtrqDefaultTaskPriority :: Lens' RegisterWorkflowType (Maybe Text)
rwtrqDefaultTaskPriority = lens _rwtrqDefaultTaskPriority (\ s a -> s{_rwtrqDefaultTaskPriority = a});

-- | If set, specifies the default maximum duration for executions of this
-- workflow type. You can override this default when starting an execution
-- through the StartWorkflowExecution Action or
-- @StartChildWorkflowExecution@ Decision.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. Unlike some of the other timeout parameters in Amazon SWF, you
-- cannot specify a value of \"NONE\" for
-- @defaultExecutionStartToCloseTimeout@; there is a one-year max limit on
-- the time that a workflow execution can run. Exceeding this limit will
-- always cause the workflow execution to time out.
rwtrqDefaultExecutionStartToCloseTimeout :: Lens' RegisterWorkflowType (Maybe Text)
rwtrqDefaultExecutionStartToCloseTimeout = lens _rwtrqDefaultExecutionStartToCloseTimeout (\ s a -> s{_rwtrqDefaultExecutionStartToCloseTimeout = a});

-- | If set, specifies the default maximum duration of decision tasks for
-- this workflow type. This default can be overridden when starting a
-- workflow execution using the StartWorkflowExecution action or the
-- @StartChildWorkflowExecution@ Decision.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
rwtrqDefaultTaskStartToCloseTimeout :: Lens' RegisterWorkflowType (Maybe Text)
rwtrqDefaultTaskStartToCloseTimeout = lens _rwtrqDefaultTaskStartToCloseTimeout (\ s a -> s{_rwtrqDefaultTaskStartToCloseTimeout = a});

-- | Textual description of the workflow type.
rwtrqDescription :: Lens' RegisterWorkflowType (Maybe Text)
rwtrqDescription = lens _rwtrqDescription (\ s a -> s{_rwtrqDescription = a});

-- | The name of the domain in which to register the workflow type.
rwtrqDomain :: Lens' RegisterWorkflowType Text
rwtrqDomain = lens _rwtrqDomain (\ s a -> s{_rwtrqDomain = a});

-- | The name of the workflow type.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (\\u0000-\\u001f | \\u007f - \\u009f). Also, it must not
-- contain the literal string quotarnquot.
rwtrqName :: Lens' RegisterWorkflowType Text
rwtrqName = lens _rwtrqName (\ s a -> s{_rwtrqName = a});

-- | The version of the workflow type.
--
-- The workflow type consists of the name and version, the combination of
-- which must be unique within the domain. To get a list of all currently
-- registered workflow types, use the ListWorkflowTypes action.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (\\u0000-\\u001f | \\u007f - \\u009f). Also, it must not
-- contain the literal string quotarnquot.
rwtrqVersion :: Lens' RegisterWorkflowType Text
rwtrqVersion = lens _rwtrqVersion (\ s a -> s{_rwtrqVersion = a});

instance AWSRequest RegisterWorkflowType where
        type Sv RegisterWorkflowType = SWF
        type Rs RegisterWorkflowType =
             RegisterWorkflowTypeResponse
        request = postJSON
        response = receiveNull RegisterWorkflowTypeResponse'

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
              ["defaultChildPolicy" .= _rwtrqDefaultChildPolicy,
               "defaultTaskList" .= _rwtrqDefaultTaskList,
               "defaultTaskPriority" .= _rwtrqDefaultTaskPriority,
               "defaultExecutionStartToCloseTimeout" .=
                 _rwtrqDefaultExecutionStartToCloseTimeout,
               "defaultTaskStartToCloseTimeout" .=
                 _rwtrqDefaultTaskStartToCloseTimeout,
               "description" .= _rwtrqDescription,
               "domain" .= _rwtrqDomain, "name" .= _rwtrqName,
               "version" .= _rwtrqVersion]

instance ToPath RegisterWorkflowType where
        toPath = const "/"

instance ToQuery RegisterWorkflowType where
        toQuery = const mempty

-- | /See:/ 'registerWorkflowTypeResponse' smart constructor.
data RegisterWorkflowTypeResponse =
    RegisterWorkflowTypeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterWorkflowTypeResponse' smart constructor.
registerWorkflowTypeResponse :: RegisterWorkflowTypeResponse
registerWorkflowTypeResponse = RegisterWorkflowTypeResponse'
