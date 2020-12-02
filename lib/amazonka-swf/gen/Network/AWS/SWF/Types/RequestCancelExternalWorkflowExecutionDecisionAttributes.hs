{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionDecisionAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @RequestCancelExternalWorkflowExecution@ decision.
--
--
-- __Access Control__
--
-- You can use IAM policies to control this decision's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
--
-- /See:/ 'requestCancelExternalWorkflowExecutionDecisionAttributes' smart constructor.
data RequestCancelExternalWorkflowExecutionDecisionAttributes = RequestCancelExternalWorkflowExecutionDecisionAttributes'
  { _rcewedaControl ::
      !( Maybe
           Text
       ),
    _rcewedaRunId ::
      !( Maybe
           Text
       ),
    _rcewedaWorkflowId ::
      !Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'RequestCancelExternalWorkflowExecutionDecisionAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcewedaControl' - The data attached to the event that can be used by the decider in subsequent workflow tasks.
--
-- * 'rcewedaRunId' - The @runId@ of the external workflow execution to cancel.
--
-- * 'rcewedaWorkflowId' - The @workflowId@ of the external workflow execution to cancel.
requestCancelExternalWorkflowExecutionDecisionAttributes ::
  -- | 'rcewedaWorkflowId'
  Text ->
  RequestCancelExternalWorkflowExecutionDecisionAttributes
requestCancelExternalWorkflowExecutionDecisionAttributes
  pWorkflowId_ =
    RequestCancelExternalWorkflowExecutionDecisionAttributes'
      { _rcewedaControl =
          Nothing,
        _rcewedaRunId = Nothing,
        _rcewedaWorkflowId = pWorkflowId_
      }

-- | The data attached to the event that can be used by the decider in subsequent workflow tasks.
rcewedaControl :: Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes (Maybe Text)
rcewedaControl = lens _rcewedaControl (\s a -> s {_rcewedaControl = a})

-- | The @runId@ of the external workflow execution to cancel.
rcewedaRunId :: Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes (Maybe Text)
rcewedaRunId = lens _rcewedaRunId (\s a -> s {_rcewedaRunId = a})

-- | The @workflowId@ of the external workflow execution to cancel.
rcewedaWorkflowId :: Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes Text
rcewedaWorkflowId = lens _rcewedaWorkflowId (\s a -> s {_rcewedaWorkflowId = a})

instance
  Hashable
    RequestCancelExternalWorkflowExecutionDecisionAttributes

instance
  NFData
    RequestCancelExternalWorkflowExecutionDecisionAttributes

instance
  ToJSON
    RequestCancelExternalWorkflowExecutionDecisionAttributes
  where
  toJSON
    RequestCancelExternalWorkflowExecutionDecisionAttributes' {..} =
      object
        ( catMaybes
            [ ("control" .=) <$> _rcewedaControl,
              ("runId" .=) <$> _rcewedaRunId,
              Just ("workflowId" .= _rcewedaWorkflowId)
            ]
        )
