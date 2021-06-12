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
-- Module      : Network.AWS.OpsWorks.Types.CloudWatchLogsTimeZone
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.CloudWatchLogsTimeZone
  ( CloudWatchLogsTimeZone
      ( ..,
        CloudWatchLogsTimeZone_LOCAL,
        CloudWatchLogsTimeZone_UTC
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The preferred time zone for logs streamed to CloudWatch Logs. Valid
-- values are @LOCAL@ and @UTC@, for Coordinated Universal Time.
newtype CloudWatchLogsTimeZone = CloudWatchLogsTimeZone'
  { fromCloudWatchLogsTimeZone ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
