{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.RetryPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.RetryPolicy where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A @RetryPolicy@ object that includes information about the retry policy settings.
--
--
--
-- /See:/ 'retryPolicy' smart constructor.
data RetryPolicy = RetryPolicy'
  { _rpMaximumEventAgeInSeconds ::
      !(Maybe Nat),
    _rpMaximumRetryAttempts :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RetryPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpMaximumEventAgeInSeconds' - The maximum amount of time, in seconds, to continue to make retry attempts.
--
-- * 'rpMaximumRetryAttempts' - The maximum number of retry attempts to make before the request fails. Retry attempts continue until either the maximum number of attempts is made or until the duration of the @MaximumEventAgeInSeconds@ is met.
retryPolicy ::
  RetryPolicy
retryPolicy =
  RetryPolicy'
    { _rpMaximumEventAgeInSeconds = Nothing,
      _rpMaximumRetryAttempts = Nothing
    }

-- | The maximum amount of time, in seconds, to continue to make retry attempts.
rpMaximumEventAgeInSeconds :: Lens' RetryPolicy (Maybe Natural)
rpMaximumEventAgeInSeconds = lens _rpMaximumEventAgeInSeconds (\s a -> s {_rpMaximumEventAgeInSeconds = a}) . mapping _Nat

-- | The maximum number of retry attempts to make before the request fails. Retry attempts continue until either the maximum number of attempts is made or until the duration of the @MaximumEventAgeInSeconds@ is met.
rpMaximumRetryAttempts :: Lens' RetryPolicy (Maybe Natural)
rpMaximumRetryAttempts = lens _rpMaximumRetryAttempts (\s a -> s {_rpMaximumRetryAttempts = a}) . mapping _Nat

instance FromJSON RetryPolicy where
  parseJSON =
    withObject
      "RetryPolicy"
      ( \x ->
          RetryPolicy'
            <$> (x .:? "MaximumEventAgeInSeconds")
            <*> (x .:? "MaximumRetryAttempts")
      )

instance Hashable RetryPolicy

instance NFData RetryPolicy

instance ToJSON RetryPolicy where
  toJSON RetryPolicy' {..} =
    object
      ( catMaybes
          [ ("MaximumEventAgeInSeconds" .=) <$> _rpMaximumEventAgeInSeconds,
            ("MaximumRetryAttempts" .=) <$> _rpMaximumRetryAttempts
          ]
      )
