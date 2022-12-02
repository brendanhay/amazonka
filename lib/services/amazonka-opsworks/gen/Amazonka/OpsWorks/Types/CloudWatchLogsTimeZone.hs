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
-- Module      : Amazonka.OpsWorks.Types.CloudWatchLogsTimeZone
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.CloudWatchLogsTimeZone
  ( CloudWatchLogsTimeZone
      ( ..,
        CloudWatchLogsTimeZone_LOCAL,
        CloudWatchLogsTimeZone_UTC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The preferred time zone for logs streamed to CloudWatch Logs. Valid
-- values are @LOCAL@ and @UTC@, for Coordinated Universal Time.
newtype CloudWatchLogsTimeZone = CloudWatchLogsTimeZone'
  { fromCloudWatchLogsTimeZone ::
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

pattern CloudWatchLogsTimeZone_LOCAL :: CloudWatchLogsTimeZone
pattern CloudWatchLogsTimeZone_LOCAL = CloudWatchLogsTimeZone' "LOCAL"

pattern CloudWatchLogsTimeZone_UTC :: CloudWatchLogsTimeZone
pattern CloudWatchLogsTimeZone_UTC = CloudWatchLogsTimeZone' "UTC"

{-# COMPLETE
  CloudWatchLogsTimeZone_LOCAL,
  CloudWatchLogsTimeZone_UTC,
  CloudWatchLogsTimeZone'
  #-}
