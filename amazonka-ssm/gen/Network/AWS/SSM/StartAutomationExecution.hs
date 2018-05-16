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
-- Module      : Network.AWS.SSM.StartAutomationExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates execution of an Automation document.
--
--
module Network.AWS.SSM.StartAutomationExecution
    (
    -- * Creating a Request
      startAutomationExecution
    , StartAutomationExecution
    -- * Request Lenses
    , saeTargetParameterName
    , saeClientToken
    , saeMode
    , saeMaxErrors
    , saeTargets
    , saeParameters
    , saeDocumentVersion
    , saeMaxConcurrency
    , saeDocumentName

    -- * Destructuring the Response
    , startAutomationExecutionResponse
    , StartAutomationExecutionResponse
    -- * Response Lenses
    , srsAutomationExecutionId
    , srsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'startAutomationExecution' smart constructor.
data StartAutomationExecution = StartAutomationExecution'
  { _saeTargetParameterName :: !(Maybe Text)
  , _saeClientToken         :: !(Maybe Text)
  , _saeMode                :: !(Maybe ExecutionMode)
  , _saeMaxErrors           :: !(Maybe Text)
  , _saeTargets             :: !(Maybe [Target])
  , _saeParameters          :: !(Maybe (Map Text [Text]))
  , _saeDocumentVersion     :: !(Maybe Text)
  , _saeMaxConcurrency      :: !(Maybe Text)
  , _saeDocumentName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartAutomationExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saeTargetParameterName' - The name of the parameter used as the target resource for the rate-controlled execution. Required if you specify Targets.
--
-- * 'saeClientToken' - User-provided idempotency token. The token must be unique, is case insensitive, enforces the UUID format, and can't be reused.
--
-- * 'saeMode' - The execution mode of the automation. Valid modes include the following: Auto and Interactive. The default mode is Auto.
--
-- * 'saeMaxErrors' - The number of errors that are allowed before the system stops running the automation on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops running the automation when the fourth error is received. If you specify 0, then the system stops running the automation on additional targets after the first error result is returned. If you run an automation on 50 resources and set max-errors to 10%, then the system stops running the automation on additional targets when the sixth error is received. Executions that are already running an automation when max-errors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set max-concurrency to 1 so the executions proceed one at a time.
--
-- * 'saeTargets' - A key-value mapping to target resources. Required if you specify TargetParameterName.
--
-- * 'saeParameters' - A key-value map of execution parameters, which match the declared parameters in the Automation document.
--
-- * 'saeDocumentVersion' - The version of the Automation document to use for this execution.
--
-- * 'saeMaxConcurrency' - The maximum number of targets allowed to run this task in parallel. You can specify a number, such as 10, or a percentage, such as 10%. The default value is 10.
--
-- * 'saeDocumentName' - The name of the Automation document to use for this execution.
startAutomationExecution
    :: Text -- ^ 'saeDocumentName'
    -> StartAutomationExecution
startAutomationExecution pDocumentName_ =
  StartAutomationExecution'
    { _saeTargetParameterName = Nothing
    , _saeClientToken = Nothing
    , _saeMode = Nothing
    , _saeMaxErrors = Nothing
    , _saeTargets = Nothing
    , _saeParameters = Nothing
    , _saeDocumentVersion = Nothing
    , _saeMaxConcurrency = Nothing
    , _saeDocumentName = pDocumentName_
    }


-- | The name of the parameter used as the target resource for the rate-controlled execution. Required if you specify Targets.
saeTargetParameterName :: Lens' StartAutomationExecution (Maybe Text)
saeTargetParameterName = lens _saeTargetParameterName (\ s a -> s{_saeTargetParameterName = a})

-- | User-provided idempotency token. The token must be unique, is case insensitive, enforces the UUID format, and can't be reused.
saeClientToken :: Lens' StartAutomationExecution (Maybe Text)
saeClientToken = lens _saeClientToken (\ s a -> s{_saeClientToken = a})

-- | The execution mode of the automation. Valid modes include the following: Auto and Interactive. The default mode is Auto.
saeMode :: Lens' StartAutomationExecution (Maybe ExecutionMode)
saeMode = lens _saeMode (\ s a -> s{_saeMode = a})

-- | The number of errors that are allowed before the system stops running the automation on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops running the automation when the fourth error is received. If you specify 0, then the system stops running the automation on additional targets after the first error result is returned. If you run an automation on 50 resources and set max-errors to 10%, then the system stops running the automation on additional targets when the sixth error is received. Executions that are already running an automation when max-errors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set max-concurrency to 1 so the executions proceed one at a time.
saeMaxErrors :: Lens' StartAutomationExecution (Maybe Text)
saeMaxErrors = lens _saeMaxErrors (\ s a -> s{_saeMaxErrors = a})

-- | A key-value mapping to target resources. Required if you specify TargetParameterName.
saeTargets :: Lens' StartAutomationExecution [Target]
saeTargets = lens _saeTargets (\ s a -> s{_saeTargets = a}) . _Default . _Coerce

-- | A key-value map of execution parameters, which match the declared parameters in the Automation document.
saeParameters :: Lens' StartAutomationExecution (HashMap Text [Text])
saeParameters = lens _saeParameters (\ s a -> s{_saeParameters = a}) . _Default . _Map

-- | The version of the Automation document to use for this execution.
saeDocumentVersion :: Lens' StartAutomationExecution (Maybe Text)
saeDocumentVersion = lens _saeDocumentVersion (\ s a -> s{_saeDocumentVersion = a})

-- | The maximum number of targets allowed to run this task in parallel. You can specify a number, such as 10, or a percentage, such as 10%. The default value is 10.
saeMaxConcurrency :: Lens' StartAutomationExecution (Maybe Text)
saeMaxConcurrency = lens _saeMaxConcurrency (\ s a -> s{_saeMaxConcurrency = a})

-- | The name of the Automation document to use for this execution.
saeDocumentName :: Lens' StartAutomationExecution Text
saeDocumentName = lens _saeDocumentName (\ s a -> s{_saeDocumentName = a})

instance AWSRequest StartAutomationExecution where
        type Rs StartAutomationExecution =
             StartAutomationExecutionResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 StartAutomationExecutionResponse' <$>
                   (x .?> "AutomationExecutionId") <*>
                     (pure (fromEnum s)))

instance Hashable StartAutomationExecution where

instance NFData StartAutomationExecution where

instance ToHeaders StartAutomationExecution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.StartAutomationExecution" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartAutomationExecution where
        toJSON StartAutomationExecution'{..}
          = object
              (catMaybes
                 [("TargetParameterName" .=) <$>
                    _saeTargetParameterName,
                  ("ClientToken" .=) <$> _saeClientToken,
                  ("Mode" .=) <$> _saeMode,
                  ("MaxErrors" .=) <$> _saeMaxErrors,
                  ("Targets" .=) <$> _saeTargets,
                  ("Parameters" .=) <$> _saeParameters,
                  ("DocumentVersion" .=) <$> _saeDocumentVersion,
                  ("MaxConcurrency" .=) <$> _saeMaxConcurrency,
                  Just ("DocumentName" .= _saeDocumentName)])

instance ToPath StartAutomationExecution where
        toPath = const "/"

instance ToQuery StartAutomationExecution where
        toQuery = const mempty

-- | /See:/ 'startAutomationExecutionResponse' smart constructor.
data StartAutomationExecutionResponse = StartAutomationExecutionResponse'
  { _srsAutomationExecutionId :: !(Maybe Text)
  , _srsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartAutomationExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsAutomationExecutionId' - The unique ID of a newly scheduled automation execution.
--
-- * 'srsResponseStatus' - -- | The response status code.
startAutomationExecutionResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StartAutomationExecutionResponse
startAutomationExecutionResponse pResponseStatus_ =
  StartAutomationExecutionResponse'
    {_srsAutomationExecutionId = Nothing, _srsResponseStatus = pResponseStatus_}


-- | The unique ID of a newly scheduled automation execution.
srsAutomationExecutionId :: Lens' StartAutomationExecutionResponse (Maybe Text)
srsAutomationExecutionId = lens _srsAutomationExecutionId (\ s a -> s{_srsAutomationExecutionId = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' StartAutomationExecutionResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData StartAutomationExecutionResponse
         where
