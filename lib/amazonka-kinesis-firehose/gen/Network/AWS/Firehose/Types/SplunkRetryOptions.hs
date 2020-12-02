{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SplunkRetryOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.SplunkRetryOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configures retry behavior in case Kinesis Data Firehose is unable to deliver documents to Splunk, or if it doesn't receive an acknowledgment from Splunk.
--
--
--
-- /See:/ 'splunkRetryOptions' smart constructor.
newtype SplunkRetryOptions = SplunkRetryOptions'
  { _sroDurationInSeconds ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SplunkRetryOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sroDurationInSeconds' - The total amount of time that Kinesis Data Firehose spends on retries. This duration starts after the initial attempt to send data to Splunk fails. It doesn't include the periods during which Kinesis Data Firehose waits for acknowledgment from Splunk after each attempt.
splunkRetryOptions ::
  SplunkRetryOptions
splunkRetryOptions =
  SplunkRetryOptions' {_sroDurationInSeconds = Nothing}

-- | The total amount of time that Kinesis Data Firehose spends on retries. This duration starts after the initial attempt to send data to Splunk fails. It doesn't include the periods during which Kinesis Data Firehose waits for acknowledgment from Splunk after each attempt.
sroDurationInSeconds :: Lens' SplunkRetryOptions (Maybe Natural)
sroDurationInSeconds = lens _sroDurationInSeconds (\s a -> s {_sroDurationInSeconds = a}) . mapping _Nat

instance FromJSON SplunkRetryOptions where
  parseJSON =
    withObject
      "SplunkRetryOptions"
      (\x -> SplunkRetryOptions' <$> (x .:? "DurationInSeconds"))

instance Hashable SplunkRetryOptions

instance NFData SplunkRetryOptions

instance ToJSON SplunkRetryOptions where
  toJSON SplunkRetryOptions' {..} =
    object
      (catMaybes [("DurationInSeconds" .=) <$> _sroDurationInSeconds])
