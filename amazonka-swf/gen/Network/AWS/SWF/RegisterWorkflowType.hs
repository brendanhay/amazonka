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
-- * 'rwtDefaultChildPolicy' - If set, specifies the default policy to use for the child workflow executions when a workflow execution of this type is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' . The supported child policies are:     * @TERMINATE@
