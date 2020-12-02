{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestGridSessionAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TestGridSessionAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An action taken by a 'TestGridSession' browser instance.
--
--
--
-- /See:/ 'testGridSessionAction' smart constructor.
data TestGridSessionAction = TestGridSessionAction'
  { _tgsaAction ::
      !(Maybe Text),
    _tgsaDuration :: !(Maybe Integer),
    _tgsaRequestMethod :: !(Maybe Text),
    _tgsaStarted :: !(Maybe POSIX),
    _tgsaStatusCode :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TestGridSessionAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgsaAction' - The action taken by the session.
--
-- * 'tgsaDuration' - The time, in milliseconds, that the action took to complete in the browser.
--
-- * 'tgsaRequestMethod' - HTTP method that the browser used to make the request.
--
-- * 'tgsaStarted' - The time that the session invoked the action.
--
-- * 'tgsaStatusCode' - HTTP status code returned to the browser when the action was taken.
testGridSessionAction ::
  TestGridSessionAction
testGridSessionAction =
  TestGridSessionAction'
    { _tgsaAction = Nothing,
      _tgsaDuration = Nothing,
      _tgsaRequestMethod = Nothing,
      _tgsaStarted = Nothing,
      _tgsaStatusCode = Nothing
    }

-- | The action taken by the session.
tgsaAction :: Lens' TestGridSessionAction (Maybe Text)
tgsaAction = lens _tgsaAction (\s a -> s {_tgsaAction = a})

-- | The time, in milliseconds, that the action took to complete in the browser.
tgsaDuration :: Lens' TestGridSessionAction (Maybe Integer)
tgsaDuration = lens _tgsaDuration (\s a -> s {_tgsaDuration = a})

-- | HTTP method that the browser used to make the request.
tgsaRequestMethod :: Lens' TestGridSessionAction (Maybe Text)
tgsaRequestMethod = lens _tgsaRequestMethod (\s a -> s {_tgsaRequestMethod = a})

-- | The time that the session invoked the action.
tgsaStarted :: Lens' TestGridSessionAction (Maybe UTCTime)
tgsaStarted = lens _tgsaStarted (\s a -> s {_tgsaStarted = a}) . mapping _Time

-- | HTTP status code returned to the browser when the action was taken.
tgsaStatusCode :: Lens' TestGridSessionAction (Maybe Text)
tgsaStatusCode = lens _tgsaStatusCode (\s a -> s {_tgsaStatusCode = a})

instance FromJSON TestGridSessionAction where
  parseJSON =
    withObject
      "TestGridSessionAction"
      ( \x ->
          TestGridSessionAction'
            <$> (x .:? "action")
            <*> (x .:? "duration")
            <*> (x .:? "requestMethod")
            <*> (x .:? "started")
            <*> (x .:? "statusCode")
      )

instance Hashable TestGridSessionAction

instance NFData TestGridSessionAction
