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
-- Module      : Network.AWS.StepFunctions.StartExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a state machine execution.
--
--
module Network.AWS.StepFunctions.StartExecution
    (
    -- * Creating a Request
      startExecution
    , StartExecution
    -- * Request Lenses
    , seInput
    , seName
    , seStateMachineARN

    -- * Destructuring the Response
    , startExecutionResponse
    , StartExecutionResponse
    -- * Response Lenses
    , srsResponseStatus
    , srsExecutionARN
    , srsStartDate
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types
import Network.AWS.StepFunctions.Types.Product

-- | /See:/ 'startExecution' smart constructor.
data StartExecution = StartExecution'
  { _seInput           :: !(Maybe Text)
  , _seName            :: !(Maybe Text)
  , _seStateMachineARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seInput' - The string that contains the JSON input data for the execution, for example: @"input": "{\"first_name\" : \"test\"}"@
--
-- * 'seName' - The name of the execution. This name must be unique for your AWS account and region for 90 days. For more information, see <http://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions> in the /AWS Step Functions Developer Guide/ . /Important:/ An execution can't use the name of another execution for 90 days. When you make multiple @StartExecution@ calls with the same name, the new execution doesn't run and the following rules apply:     * When the original execution is open and the execution input from the new call is /different/ , the @ExecutionAlreadyExists@ message is returned.     * When the original execution is open and the execution input from the new call is /identical/ , the @Success@ message is returned.     * When the original execution is closed, the @ExecutionAlreadyExists@ message is returned regardless of input. A name must /not/ contain:     * whitespace     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
-- * 'seStateMachineARN' - The Amazon Resource Name (ARN) of the state machine to execute.
startExecution
    :: Text -- ^ 'seStateMachineARN'
    -> StartExecution
startExecution pStateMachineARN_ =
  StartExecution'
    { _seInput = Nothing
    , _seName = Nothing
    , _seStateMachineARN = pStateMachineARN_
    }


-- | The string that contains the JSON input data for the execution, for example: @"input": "{\"first_name\" : \"test\"}"@
seInput :: Lens' StartExecution (Maybe Text)
seInput = lens _seInput (\ s a -> s{_seInput = a})

-- | The name of the execution. This name must be unique for your AWS account and region for 90 days. For more information, see <http://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions> in the /AWS Step Functions Developer Guide/ . /Important:/ An execution can't use the name of another execution for 90 days. When you make multiple @StartExecution@ calls with the same name, the new execution doesn't run and the following rules apply:     * When the original execution is open and the execution input from the new call is /different/ , the @ExecutionAlreadyExists@ message is returned.     * When the original execution is open and the execution input from the new call is /identical/ , the @Success@ message is returned.     * When the original execution is closed, the @ExecutionAlreadyExists@ message is returned regardless of input. A name must /not/ contain:     * whitespace     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ )
seName :: Lens' StartExecution (Maybe Text)
seName = lens _seName (\ s a -> s{_seName = a})

-- | The Amazon Resource Name (ARN) of the state machine to execute.
seStateMachineARN :: Lens' StartExecution Text
seStateMachineARN = lens _seStateMachineARN (\ s a -> s{_seStateMachineARN = a})

instance AWSRequest StartExecution where
        type Rs StartExecution = StartExecutionResponse
        request = postJSON stepFunctions
        response
          = receiveJSON
              (\ s h x ->
                 StartExecutionResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "executionArn") <*>
                     (x .:> "startDate"))

instance Hashable StartExecution where

instance NFData StartExecution where

instance ToHeaders StartExecution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSStepFunctions.StartExecution" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON StartExecution where
        toJSON StartExecution'{..}
          = object
              (catMaybes
                 [("input" .=) <$> _seInput, ("name" .=) <$> _seName,
                  Just ("stateMachineArn" .= _seStateMachineARN)])

instance ToPath StartExecution where
        toPath = const "/"

instance ToQuery StartExecution where
        toQuery = const mempty

-- | /See:/ 'startExecutionResponse' smart constructor.
data StartExecutionResponse = StartExecutionResponse'
  { _srsResponseStatus :: !Int
  , _srsExecutionARN   :: !Text
  , _srsStartDate      :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsResponseStatus' - -- | The response status code.
--
-- * 'srsExecutionARN' - The Amazon Resource Name (ARN) that identifies the execution.
--
-- * 'srsStartDate' - The date the execution is started.
startExecutionResponse
    :: Int -- ^ 'srsResponseStatus'
    -> Text -- ^ 'srsExecutionARN'
    -> UTCTime -- ^ 'srsStartDate'
    -> StartExecutionResponse
startExecutionResponse pResponseStatus_ pExecutionARN_ pStartDate_ =
  StartExecutionResponse'
    { _srsResponseStatus = pResponseStatus_
    , _srsExecutionARN = pExecutionARN_
    , _srsStartDate = _Time # pStartDate_
    }


-- | -- | The response status code.
srsResponseStatus :: Lens' StartExecutionResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

-- | The Amazon Resource Name (ARN) that identifies the execution.
srsExecutionARN :: Lens' StartExecutionResponse Text
srsExecutionARN = lens _srsExecutionARN (\ s a -> s{_srsExecutionARN = a})

-- | The date the execution is started.
srsStartDate :: Lens' StartExecutionResponse UTCTime
srsStartDate = lens _srsStartDate (\ s a -> s{_srsStartDate = a}) . _Time

instance NFData StartExecutionResponse where
