{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.ThrottleSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.ThrottleSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The API request rate limits.
--
--
--
-- /See:/ 'throttleSettings' smart constructor.
data ThrottleSettings = ThrottleSettings'
  { _tsBurstLimit ::
      !(Maybe Int),
    _tsRateLimit :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ThrottleSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsBurstLimit' - The API request burst limit, the maximum rate limit over a time ranging from one to a few seconds, depending upon whether the underlying token bucket is at its full capacity.
--
-- * 'tsRateLimit' - The API request steady-state rate limit.
throttleSettings ::
  ThrottleSettings
throttleSettings =
  ThrottleSettings'
    { _tsBurstLimit = Nothing,
      _tsRateLimit = Nothing
    }

-- | The API request burst limit, the maximum rate limit over a time ranging from one to a few seconds, depending upon whether the underlying token bucket is at its full capacity.
tsBurstLimit :: Lens' ThrottleSettings (Maybe Int)
tsBurstLimit = lens _tsBurstLimit (\s a -> s {_tsBurstLimit = a})

-- | The API request steady-state rate limit.
tsRateLimit :: Lens' ThrottleSettings (Maybe Double)
tsRateLimit = lens _tsRateLimit (\s a -> s {_tsRateLimit = a})

instance FromJSON ThrottleSettings where
  parseJSON =
    withObject
      "ThrottleSettings"
      ( \x ->
          ThrottleSettings' <$> (x .:? "burstLimit") <*> (x .:? "rateLimit")
      )

instance Hashable ThrottleSettings

instance NFData ThrottleSettings

instance ToJSON ThrottleSettings where
  toJSON ThrottleSettings' {..} =
    object
      ( catMaybes
          [ ("burstLimit" .=) <$> _tsBurstLimit,
            ("rateLimit" .=) <$> _tsRateLimit
          ]
      )
