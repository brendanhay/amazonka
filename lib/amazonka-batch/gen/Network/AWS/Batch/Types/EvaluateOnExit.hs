{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.EvaluateOnExit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.EvaluateOnExit where

import Network.AWS.Batch.Types.RetryAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a set of conditions to be met, and an action to take (@RETRY@ or @EXIT@ ) if all conditions are met.
--
--
--
-- /See:/ 'evaluateOnExit' smart constructor.
data EvaluateOnExit = EvaluateOnExit'
  { _eoeOnExitCode ::
      !(Maybe Text),
    _eoeOnReason :: !(Maybe Text),
    _eoeOnStatusReason :: !(Maybe Text),
    _eoeAction :: !RetryAction
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EvaluateOnExit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eoeOnExitCode' - Contains a glob pattern to match against the decimal representation of the @ExitCode@ returned for a job. The patten can be up to 512 characters long, can contain only numbers, and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
--
-- * 'eoeOnReason' - Contains a glob pattern to match against the @Reason@ returned for a job. The patten can be up to 512 characters long, can contain letters, numbers, periods (.), colons (:), and whitespace (spaces, tabs), and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
--
-- * 'eoeOnStatusReason' - Contains a glob pattern to match against the @StatusReason@ returned for a job. The patten can be up to 512 characters long, can contain letters, numbers, periods (.), colons (:), and whitespace (spaces, tabs). and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
--
-- * 'eoeAction' - Specifies the action to take if all of the specified conditions (@onStatusReason@ , @onReason@ , and @onExitCode@ ) are met.
evaluateOnExit ::
  -- | 'eoeAction'
  RetryAction ->
  EvaluateOnExit
evaluateOnExit pAction_ =
  EvaluateOnExit'
    { _eoeOnExitCode = Nothing,
      _eoeOnReason = Nothing,
      _eoeOnStatusReason = Nothing,
      _eoeAction = pAction_
    }

-- | Contains a glob pattern to match against the decimal representation of the @ExitCode@ returned for a job. The patten can be up to 512 characters long, can contain only numbers, and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
eoeOnExitCode :: Lens' EvaluateOnExit (Maybe Text)
eoeOnExitCode = lens _eoeOnExitCode (\s a -> s {_eoeOnExitCode = a})

-- | Contains a glob pattern to match against the @Reason@ returned for a job. The patten can be up to 512 characters long, can contain letters, numbers, periods (.), colons (:), and whitespace (spaces, tabs), and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
eoeOnReason :: Lens' EvaluateOnExit (Maybe Text)
eoeOnReason = lens _eoeOnReason (\s a -> s {_eoeOnReason = a})

-- | Contains a glob pattern to match against the @StatusReason@ returned for a job. The patten can be up to 512 characters long, can contain letters, numbers, periods (.), colons (:), and whitespace (spaces, tabs). and can optionally end with an asterisk (*) so that only the start of the string needs to be an exact match.
eoeOnStatusReason :: Lens' EvaluateOnExit (Maybe Text)
eoeOnStatusReason = lens _eoeOnStatusReason (\s a -> s {_eoeOnStatusReason = a})

-- | Specifies the action to take if all of the specified conditions (@onStatusReason@ , @onReason@ , and @onExitCode@ ) are met.
eoeAction :: Lens' EvaluateOnExit RetryAction
eoeAction = lens _eoeAction (\s a -> s {_eoeAction = a})

instance FromJSON EvaluateOnExit where
  parseJSON =
    withObject
      "EvaluateOnExit"
      ( \x ->
          EvaluateOnExit'
            <$> (x .:? "onExitCode")
            <*> (x .:? "onReason")
            <*> (x .:? "onStatusReason")
            <*> (x .: "action")
      )

instance Hashable EvaluateOnExit

instance NFData EvaluateOnExit

instance ToJSON EvaluateOnExit where
  toJSON EvaluateOnExit' {..} =
    object
      ( catMaybes
          [ ("onExitCode" .=) <$> _eoeOnExitCode,
            ("onReason" .=) <$> _eoeOnReason,
            ("onStatusReason" .=) <$> _eoeOnStatusReason,
            Just ("action" .= _eoeAction)
          ]
      )
