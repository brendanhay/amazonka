{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.RedshiftRetryOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.RedshiftRetryOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configures retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift.
--
--
--
-- /See:/ 'redshiftRetryOptions' smart constructor.
newtype RedshiftRetryOptions = RedshiftRetryOptions'
  { _rroDurationInSeconds ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RedshiftRetryOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rroDurationInSeconds' - The length of time during which Kinesis Data Firehose retries delivery after a failure, starting from the initial request and including the first attempt. The default value is 3600 seconds (60 minutes). Kinesis Data Firehose does not retry if the value of @DurationInSeconds@ is 0 (zero) or if the first delivery attempt takes longer than the current value.
redshiftRetryOptions ::
  RedshiftRetryOptions
redshiftRetryOptions =
  RedshiftRetryOptions' {_rroDurationInSeconds = Nothing}

-- | The length of time during which Kinesis Data Firehose retries delivery after a failure, starting from the initial request and including the first attempt. The default value is 3600 seconds (60 minutes). Kinesis Data Firehose does not retry if the value of @DurationInSeconds@ is 0 (zero) or if the first delivery attempt takes longer than the current value.
rroDurationInSeconds :: Lens' RedshiftRetryOptions (Maybe Natural)
rroDurationInSeconds = lens _rroDurationInSeconds (\s a -> s {_rroDurationInSeconds = a}) . mapping _Nat

instance FromJSON RedshiftRetryOptions where
  parseJSON =
    withObject
      "RedshiftRetryOptions"
      (\x -> RedshiftRetryOptions' <$> (x .:? "DurationInSeconds"))

instance Hashable RedshiftRetryOptions

instance NFData RedshiftRetryOptions

instance ToJSON RedshiftRetryOptions where
  toJSON RedshiftRetryOptions' {..} =
    object
      (catMaybes [("DurationInSeconds" .=) <$> _rroDurationInSeconds])
