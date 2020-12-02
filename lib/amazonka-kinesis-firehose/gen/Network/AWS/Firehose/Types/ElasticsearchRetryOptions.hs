{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ElasticsearchRetryOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ElasticsearchRetryOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configures retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon ES.
--
--
--
-- /See:/ 'elasticsearchRetryOptions' smart constructor.
newtype ElasticsearchRetryOptions = ElasticsearchRetryOptions'
  { _eroDurationInSeconds ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElasticsearchRetryOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eroDurationInSeconds' - After an initial failure to deliver to Amazon ES, the total amount of time during which Kinesis Data Firehose retries delivery (including the first attempt). After this time has elapsed, the failed documents are written to Amazon S3. Default value is 300 seconds (5 minutes). A value of 0 (zero) results in no retries.
elasticsearchRetryOptions ::
  ElasticsearchRetryOptions
elasticsearchRetryOptions =
  ElasticsearchRetryOptions' {_eroDurationInSeconds = Nothing}

-- | After an initial failure to deliver to Amazon ES, the total amount of time during which Kinesis Data Firehose retries delivery (including the first attempt). After this time has elapsed, the failed documents are written to Amazon S3. Default value is 300 seconds (5 minutes). A value of 0 (zero) results in no retries.
eroDurationInSeconds :: Lens' ElasticsearchRetryOptions (Maybe Natural)
eroDurationInSeconds = lens _eroDurationInSeconds (\s a -> s {_eroDurationInSeconds = a}) . mapping _Nat

instance FromJSON ElasticsearchRetryOptions where
  parseJSON =
    withObject
      "ElasticsearchRetryOptions"
      (\x -> ElasticsearchRetryOptions' <$> (x .:? "DurationInSeconds"))

instance Hashable ElasticsearchRetryOptions

instance NFData ElasticsearchRetryOptions

instance ToJSON ElasticsearchRetryOptions where
  toJSON ElasticsearchRetryOptions' {..} =
    object
      (catMaybes [("DurationInSeconds" .=) <$> _eroDurationInSeconds])
