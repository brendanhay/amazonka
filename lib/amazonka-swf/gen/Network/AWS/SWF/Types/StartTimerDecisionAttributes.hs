{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartTimerDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.StartTimerDecisionAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the details of the @StartTimer@ decision.
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
-- /See:/ 'startTimerDecisionAttributes' smart constructor.
data StartTimerDecisionAttributes = StartTimerDecisionAttributes'
  { _stdaControl ::
      !(Maybe Text),
    _stdaTimerId :: !Text,
    _stdaStartToFireTimeout :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartTimerDecisionAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stdaControl' - The data attached to the event that can be used by the decider in subsequent workflow tasks.
--
-- * 'stdaTimerId' - The unique ID of the timer. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
--
-- * 'stdaStartToFireTimeout' - The duration to wait before firing the timer. The duration is specified in seconds, an integer greater than or equal to @0@ .
startTimerDecisionAttributes ::
  -- | 'stdaTimerId'
  Text ->
  -- | 'stdaStartToFireTimeout'
  Text ->
  StartTimerDecisionAttributes
startTimerDecisionAttributes pTimerId_ pStartToFireTimeout_ =
  StartTimerDecisionAttributes'
    { _stdaControl = Nothing,
      _stdaTimerId = pTimerId_,
      _stdaStartToFireTimeout = pStartToFireTimeout_
    }

-- | The data attached to the event that can be used by the decider in subsequent workflow tasks.
stdaControl :: Lens' StartTimerDecisionAttributes (Maybe Text)
stdaControl = lens _stdaControl (\s a -> s {_stdaControl = a})

-- | The unique ID of the timer. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
stdaTimerId :: Lens' StartTimerDecisionAttributes Text
stdaTimerId = lens _stdaTimerId (\s a -> s {_stdaTimerId = a})

-- | The duration to wait before firing the timer. The duration is specified in seconds, an integer greater than or equal to @0@ .
stdaStartToFireTimeout :: Lens' StartTimerDecisionAttributes Text
stdaStartToFireTimeout = lens _stdaStartToFireTimeout (\s a -> s {_stdaStartToFireTimeout = a})

instance Hashable StartTimerDecisionAttributes

instance NFData StartTimerDecisionAttributes

instance ToJSON StartTimerDecisionAttributes where
  toJSON StartTimerDecisionAttributes' {..} =
    object
      ( catMaybes
          [ ("control" .=) <$> _stdaControl,
            Just ("timerId" .= _stdaTimerId),
            Just ("startToFireTimeout" .= _stdaStartToFireTimeout)
          ]
      )
