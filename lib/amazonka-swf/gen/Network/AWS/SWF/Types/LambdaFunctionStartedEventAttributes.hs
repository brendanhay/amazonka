{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.LambdaFunctionStartedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionStartedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @LambdaFunctionStarted@ event. It isn't set for other event types.
--
--
--
-- /See:/ 'lambdaFunctionStartedEventAttributes' smart constructor.
newtype LambdaFunctionStartedEventAttributes = LambdaFunctionStartedEventAttributes'
  { _lfseaScheduledEventId ::
      Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaFunctionStartedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfseaScheduledEventId' - The ID of the @LambdaFunctionScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
lambdaFunctionStartedEventAttributes ::
  -- | 'lfseaScheduledEventId'
  Integer ->
  LambdaFunctionStartedEventAttributes
lambdaFunctionStartedEventAttributes pScheduledEventId_ =
  LambdaFunctionStartedEventAttributes'
    { _lfseaScheduledEventId =
        pScheduledEventId_
    }

-- | The ID of the @LambdaFunctionScheduled@ event that was recorded when this activity task was scheduled. To help diagnose issues, use this information to trace back the chain of events leading up to this event.
lfseaScheduledEventId :: Lens' LambdaFunctionStartedEventAttributes Integer
lfseaScheduledEventId = lens _lfseaScheduledEventId (\s a -> s {_lfseaScheduledEventId = a})

instance FromJSON LambdaFunctionStartedEventAttributes where
  parseJSON =
    withObject
      "LambdaFunctionStartedEventAttributes"
      ( \x ->
          LambdaFunctionStartedEventAttributes'
            <$> (x .: "scheduledEventId")
      )

instance Hashable LambdaFunctionStartedEventAttributes

instance NFData LambdaFunctionStartedEventAttributes
