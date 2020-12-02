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
-- Module      : Network.AWS.StepFunctions.StopExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an execution.
--
--
module Network.AWS.StepFunctions.StopExecution
    (
    -- * Creating a Request
      stopExecution
    , StopExecution
    -- * Request Lenses
    , seError
    , seCause
    , seExecutionARN

    -- * Destructuring the Response
    , stopExecutionResponse
    , StopExecutionResponse
    -- * Response Lenses
    , sersResponseStatus
    , sersStopDate
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types
import Network.AWS.StepFunctions.Types.Product

-- | /See:/ 'stopExecution' smart constructor.
data StopExecution = StopExecution'
  { _seError        :: !(Maybe Text)
  , _seCause        :: !(Maybe Text)
  , _seExecutionARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seError' - An arbitrary error code that identifies the cause of the termination.
--
-- * 'seCause' - A more detailed explanation of the cause of the termination.
--
-- * 'seExecutionARN' - The Amazon Resource Name (ARN) of the execution to stop.
stopExecution
    :: Text -- ^ 'seExecutionARN'
    -> StopExecution
stopExecution pExecutionARN_ =
  StopExecution'
    {_seError = Nothing, _seCause = Nothing, _seExecutionARN = pExecutionARN_}


-- | An arbitrary error code that identifies the cause of the termination.
seError :: Lens' StopExecution (Maybe Text)
seError = lens _seError (\ s a -> s{_seError = a})

-- | A more detailed explanation of the cause of the termination.
seCause :: Lens' StopExecution (Maybe Text)
seCause = lens _seCause (\ s a -> s{_seCause = a})

-- | The Amazon Resource Name (ARN) of the execution to stop.
seExecutionARN :: Lens' StopExecution Text
seExecutionARN = lens _seExecutionARN (\ s a -> s{_seExecutionARN = a})

instance AWSRequest StopExecution where
        type Rs StopExecution = StopExecutionResponse
        request = postJSON stepFunctions
        response
          = receiveJSON
              (\ s h x ->
                 StopExecutionResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "stopDate"))

instance Hashable StopExecution where

instance NFData StopExecution where

instance ToHeaders StopExecution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSStepFunctions.StopExecution" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON StopExecution where
        toJSON StopExecution'{..}
          = object
              (catMaybes
                 [("error" .=) <$> _seError,
                  ("cause" .=) <$> _seCause,
                  Just ("executionArn" .= _seExecutionARN)])

instance ToPath StopExecution where
        toPath = const "/"

instance ToQuery StopExecution where
        toQuery = const mempty

-- | /See:/ 'stopExecutionResponse' smart constructor.
data StopExecutionResponse = StopExecutionResponse'
  { _sersResponseStatus :: !Int
  , _sersStopDate       :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sersResponseStatus' - -- | The response status code.
--
-- * 'sersStopDate' - The date the execution is stopped.
stopExecutionResponse
    :: Int -- ^ 'sersResponseStatus'
    -> UTCTime -- ^ 'sersStopDate'
    -> StopExecutionResponse
stopExecutionResponse pResponseStatus_ pStopDate_ =
  StopExecutionResponse'
    {_sersResponseStatus = pResponseStatus_, _sersStopDate = _Time # pStopDate_}


-- | -- | The response status code.
sersResponseStatus :: Lens' StopExecutionResponse Int
sersResponseStatus = lens _sersResponseStatus (\ s a -> s{_sersResponseStatus = a})

-- | The date the execution is stopped.
sersStopDate :: Lens' StopExecutionResponse UTCTime
sersStopDate = lens _sersStopDate (\ s a -> s{_sersStopDate = a}) . _Time

instance NFData StopExecutionResponse where
