{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointRetryOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointRetryOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
--
--
--
-- /See:/ 'hTTPEndpointRetryOptions' smart constructor.
newtype HTTPEndpointRetryOptions = HTTPEndpointRetryOptions'
  { _httperoDurationInSeconds ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPEndpointRetryOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httperoDurationInSeconds' - The total amount of time that Kinesis Data Firehose spends on retries. This duration starts after the initial attempt to send data to the custom destination via HTTPS endpoint fails. It doesn't include the periods during which Kinesis Data Firehose waits for acknowledgment from the specified destination after each attempt.
hTTPEndpointRetryOptions ::
  HTTPEndpointRetryOptions
hTTPEndpointRetryOptions =
  HTTPEndpointRetryOptions' {_httperoDurationInSeconds = Nothing}

-- | The total amount of time that Kinesis Data Firehose spends on retries. This duration starts after the initial attempt to send data to the custom destination via HTTPS endpoint fails. It doesn't include the periods during which Kinesis Data Firehose waits for acknowledgment from the specified destination after each attempt.
httperoDurationInSeconds :: Lens' HTTPEndpointRetryOptions (Maybe Natural)
httperoDurationInSeconds = lens _httperoDurationInSeconds (\s a -> s {_httperoDurationInSeconds = a}) . mapping _Nat

instance FromJSON HTTPEndpointRetryOptions where
  parseJSON =
    withObject
      "HTTPEndpointRetryOptions"
      (\x -> HTTPEndpointRetryOptions' <$> (x .:? "DurationInSeconds"))

instance Hashable HTTPEndpointRetryOptions

instance NFData HTTPEndpointRetryOptions

instance ToJSON HTTPEndpointRetryOptions where
  toJSON HTTPEndpointRetryOptions' {..} =
    object
      ( catMaybes
          [("DurationInSeconds" .=) <$> _httperoDurationInSeconds]
      )
