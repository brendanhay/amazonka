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
-- Module      : Network.AWS.StepFunctions.DescribeExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an execution.
--
--
module Network.AWS.StepFunctions.DescribeExecution
    (
    -- * Creating a Request
      describeExecution
    , DescribeExecution
    -- * Request Lenses
    , deExecutionARN

    -- * Destructuring the Response
    , describeExecutionResponse
    , DescribeExecutionResponse
    -- * Response Lenses
    , dersStopDate
    , dersName
    , dersOutput
    , dersResponseStatus
    , dersExecutionARN
    , dersStateMachineARN
    , dersStatus
    , dersStartDate
    , dersInput
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types
import Network.AWS.StepFunctions.Types.Product

-- | /See:/ 'describeExecution' smart constructor.
newtype DescribeExecution = DescribeExecution'
  { _deExecutionARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deExecutionARN' - The Amazon Resource Name (ARN) of the execution to describe.
describeExecution
    :: Text -- ^ 'deExecutionARN'
    -> DescribeExecution
describeExecution pExecutionARN_ =
  DescribeExecution' {_deExecutionARN = pExecutionARN_}


-- | The Amazon Resource Name (ARN) of the execution to describe.
deExecutionARN :: Lens' DescribeExecution Text
deExecutionARN = lens _deExecutionARN (\ s a -> s{_deExecutionARN = a})

instance AWSRequest DescribeExecution where
        type Rs DescribeExecution = DescribeExecutionResponse
        request = postJSON stepFunctions
        response
          = receiveJSON
              (\ s h x ->
                 DescribeExecutionResponse' <$>
                   (x .?> "stopDate") <*> (x .?> "name") <*>
                     (x .?> "output")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "executionArn")
                     <*> (x .:> "stateMachineArn")
                     <*> (x .:> "status")
                     <*> (x .:> "startDate")
                     <*> (x .:> "input"))

instance Hashable DescribeExecution where

instance NFData DescribeExecution where

instance ToHeaders DescribeExecution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSStepFunctions.DescribeExecution" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DescribeExecution where
        toJSON DescribeExecution'{..}
          = object
              (catMaybes
                 [Just ("executionArn" .= _deExecutionARN)])

instance ToPath DescribeExecution where
        toPath = const "/"

instance ToQuery DescribeExecution where
        toQuery = const mempty

-- | /See:/ 'describeExecutionResponse' smart constructor.
data DescribeExecutionResponse = DescribeExecutionResponse'
  { _dersStopDate        :: !(Maybe POSIX)
  , _dersName            :: !(Maybe Text)
  , _dersOutput          :: !(Maybe Text)
  , _dersResponseStatus  :: !Int
  , _dersExecutionARN    :: !Text
  , _dersStateMachineARN :: !Text
  , _dersStatus          :: !ExecutionStatus
  , _dersStartDate       :: !POSIX
  , _dersInput           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dersStopDate' - If the execution has already ended, the date the execution stopped.
--
-- * 'dersName' - The name of the execution. A name must /not/ contain:     * whitespace     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
-- * 'dersOutput' - The JSON output data of the execution.
--
-- * 'dersResponseStatus' - -- | The response status code.
--
-- * 'dersExecutionARN' - The Amazon Resource Name (ARN) that identifies the execution.
--
-- * 'dersStateMachineARN' - The Amazon Resource Name (ARN) of the executed stated machine.
--
-- * 'dersStatus' - The current status of the execution.
--
-- * 'dersStartDate' - The date the execution is started.
--
-- * 'dersInput' - The string that contains the JSON input data of the execution.
describeExecutionResponse
    :: Int -- ^ 'dersResponseStatus'
    -> Text -- ^ 'dersExecutionARN'
    -> Text -- ^ 'dersStateMachineARN'
    -> ExecutionStatus -- ^ 'dersStatus'
    -> UTCTime -- ^ 'dersStartDate'
    -> Text -- ^ 'dersInput'
    -> DescribeExecutionResponse
describeExecutionResponse pResponseStatus_ pExecutionARN_ pStateMachineARN_ pStatus_ pStartDate_ pInput_ =
  DescribeExecutionResponse'
    { _dersStopDate = Nothing
    , _dersName = Nothing
    , _dersOutput = Nothing
    , _dersResponseStatus = pResponseStatus_
    , _dersExecutionARN = pExecutionARN_
    , _dersStateMachineARN = pStateMachineARN_
    , _dersStatus = pStatus_
    , _dersStartDate = _Time # pStartDate_
    , _dersInput = pInput_
    }


-- | If the execution has already ended, the date the execution stopped.
dersStopDate :: Lens' DescribeExecutionResponse (Maybe UTCTime)
dersStopDate = lens _dersStopDate (\ s a -> s{_dersStopDate = a}) . mapping _Time

-- | The name of the execution. A name must /not/ contain:     * whitespace     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ )
dersName :: Lens' DescribeExecutionResponse (Maybe Text)
dersName = lens _dersName (\ s a -> s{_dersName = a})

-- | The JSON output data of the execution.
dersOutput :: Lens' DescribeExecutionResponse (Maybe Text)
dersOutput = lens _dersOutput (\ s a -> s{_dersOutput = a})

-- | -- | The response status code.
dersResponseStatus :: Lens' DescribeExecutionResponse Int
dersResponseStatus = lens _dersResponseStatus (\ s a -> s{_dersResponseStatus = a})

-- | The Amazon Resource Name (ARN) that identifies the execution.
dersExecutionARN :: Lens' DescribeExecutionResponse Text
dersExecutionARN = lens _dersExecutionARN (\ s a -> s{_dersExecutionARN = a})

-- | The Amazon Resource Name (ARN) of the executed stated machine.
dersStateMachineARN :: Lens' DescribeExecutionResponse Text
dersStateMachineARN = lens _dersStateMachineARN (\ s a -> s{_dersStateMachineARN = a})

-- | The current status of the execution.
dersStatus :: Lens' DescribeExecutionResponse ExecutionStatus
dersStatus = lens _dersStatus (\ s a -> s{_dersStatus = a})

-- | The date the execution is started.
dersStartDate :: Lens' DescribeExecutionResponse UTCTime
dersStartDate = lens _dersStartDate (\ s a -> s{_dersStartDate = a}) . _Time

-- | The string that contains the JSON input data of the execution.
dersInput :: Lens' DescribeExecutionResponse Text
dersInput = lens _dersInput (\ s a -> s{_dersInput = a})

instance NFData DescribeExecutionResponse where
