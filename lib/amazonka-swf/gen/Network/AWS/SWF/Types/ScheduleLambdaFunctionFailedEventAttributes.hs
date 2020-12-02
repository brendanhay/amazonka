{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedCause

-- | Provides the details of the @ScheduleLambdaFunctionFailed@ event. It isn't set for other event types.
--
--
--
-- /See:/ 'scheduleLambdaFunctionFailedEventAttributes' smart constructor.
data ScheduleLambdaFunctionFailedEventAttributes = ScheduleLambdaFunctionFailedEventAttributes'
  { _slffeaId ::
      !Text,
    _slffeaName ::
      !Text,
    _slffeaCause ::
      !ScheduleLambdaFunctionFailedCause,
    _slffeaDecisionTaskCompletedEventId ::
      !Integer
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ScheduleLambdaFunctionFailedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slffeaId' - The ID provided in the @ScheduleLambdaFunction@ decision that failed.
--
-- * 'slffeaName' - The name of the Lambda function.
--
-- * 'slffeaCause' - The cause of the failure. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
--
-- * 'slffeaDecisionTaskCompletedEventId' - The ID of the @LambdaFunctionCompleted@ event corresponding to the decision that resulted in scheduling this Lambda task. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
scheduleLambdaFunctionFailedEventAttributes ::
  -- | 'slffeaId'
  Text ->
  -- | 'slffeaName'
  Text ->
  -- | 'slffeaCause'
  ScheduleLambdaFunctionFailedCause ->
  -- | 'slffeaDecisionTaskCompletedEventId'
  Integer ->
  ScheduleLambdaFunctionFailedEventAttributes
scheduleLambdaFunctionFailedEventAttributes
  pId_
  pName_
  pCause_
  pDecisionTaskCompletedEventId_ =
    ScheduleLambdaFunctionFailedEventAttributes'
      { _slffeaId = pId_,
        _slffeaName = pName_,
        _slffeaCause = pCause_,
        _slffeaDecisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The ID provided in the @ScheduleLambdaFunction@ decision that failed.
slffeaId :: Lens' ScheduleLambdaFunctionFailedEventAttributes Text
slffeaId = lens _slffeaId (\s a -> s {_slffeaId = a})

-- | The name of the Lambda function.
slffeaName :: Lens' ScheduleLambdaFunctionFailedEventAttributes Text
slffeaName = lens _slffeaName (\s a -> s {_slffeaName = a})

-- | The cause of the failure. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
slffeaCause :: Lens' ScheduleLambdaFunctionFailedEventAttributes ScheduleLambdaFunctionFailedCause
slffeaCause = lens _slffeaCause (\s a -> s {_slffeaCause = a})

-- | The ID of the @LambdaFunctionCompleted@ event corresponding to the decision that resulted in scheduling this Lambda task. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
slffeaDecisionTaskCompletedEventId :: Lens' ScheduleLambdaFunctionFailedEventAttributes Integer
slffeaDecisionTaskCompletedEventId = lens _slffeaDecisionTaskCompletedEventId (\s a -> s {_slffeaDecisionTaskCompletedEventId = a})

instance FromJSON ScheduleLambdaFunctionFailedEventAttributes where
  parseJSON =
    withObject
      "ScheduleLambdaFunctionFailedEventAttributes"
      ( \x ->
          ScheduleLambdaFunctionFailedEventAttributes'
            <$> (x .: "id")
            <*> (x .: "name")
            <*> (x .: "cause")
            <*> (x .: "decisionTaskCompletedEventId")
      )

instance Hashable ScheduleLambdaFunctionFailedEventAttributes

instance NFData ScheduleLambdaFunctionFailedEventAttributes
