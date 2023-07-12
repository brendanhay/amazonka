{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types.LogDestinationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LogDestinationType
  ( LogDestinationType
      ( ..,
        LogDestinationType_Cloud_watch_logs,
        LogDestinationType_Kinesis_data_firehose,
        LogDestinationType_S3
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype LogDestinationType = LogDestinationType'
  { fromLogDestinationType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern LogDestinationType_Cloud_watch_logs :: LogDestinationType
pattern LogDestinationType_Cloud_watch_logs = LogDestinationType' "cloud-watch-logs"

pattern LogDestinationType_Kinesis_data_firehose :: LogDestinationType
pattern LogDestinationType_Kinesis_data_firehose = LogDestinationType' "kinesis-data-firehose"

pattern LogDestinationType_S3 :: LogDestinationType
pattern LogDestinationType_S3 = LogDestinationType' "s3"

{-# COMPLETE
  LogDestinationType_Cloud_watch_logs,
  LogDestinationType_Kinesis_data_firehose,
  LogDestinationType_S3,
  LogDestinationType'
  #-}
