{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartLambdaFunctionFailedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.StartLambdaFunctionFailedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.StartLambdaFunctionFailedCause

-- | Provides the details of the @StartLambdaFunctionFailed@ event. It isn't set for other event types.
--
--
--
-- /See:/ 'startLambdaFunctionFailedEventAttributes' smart constructor.
data StartLambdaFunctionFailedEventAttributes = StartLambdaFunctionFailedEventAttributes'
  { _sScheduledEventId ::
      !( Maybe
           Integer
       ),
    _sCause ::
      !( Maybe
           StartLambdaFunctionFailedCause
       ),
    _sMessage ::
      !( Maybe
           Text
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartLambdaFunctionFailedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sScheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- * 'sCause' - The cause of the failure. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- * 'sMessage' - A description that can help diagnose the cause of the fault.
startLambdaFunctionFailedEventAttributes ::
  StartLambdaFunctionFailedEventAttributes
startLambdaFunctionFailedEventAttributes =
  StartLambdaFunctionFailedEventAttributes'
    { _sScheduledEventId =
        Nothing,
      _sCause = Nothing,
      _sMessage = Nothing
    }

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
sScheduledEventId :: Lens' StartLambdaFunctionFailedEventAttributes (Maybe Integer)
sScheduledEventId = lens _sScheduledEventId (\s a -> s {_sScheduledEventId = a})

-- | The cause of the failure. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
sCause :: Lens' StartLambdaFunctionFailedEventAttributes (Maybe StartLambdaFunctionFailedCause)
sCause = lens _sCause (\s a -> s {_sCause = a})

-- | A description that can help diagnose the cause of the fault.
sMessage :: Lens' StartLambdaFunctionFailedEventAttributes (Maybe Text)
sMessage = lens _sMessage (\s a -> s {_sMessage = a})

instance FromJSON StartLambdaFunctionFailedEventAttributes where
  parseJSON =
    withObject
      "StartLambdaFunctionFailedEventAttributes"
      ( \x ->
          StartLambdaFunctionFailedEventAttributes'
            <$> (x .:? "scheduledEventId")
            <*> (x .:? "cause")
            <*> (x .:? "message")
      )

instance Hashable StartLambdaFunctionFailedEventAttributes

instance NFData StartLambdaFunctionFailedEventAttributes
