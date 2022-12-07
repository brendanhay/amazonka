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
-- Module      : Amazonka.MediaConvert.Types.StatusUpdateInterval
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.StatusUpdateInterval
  ( StatusUpdateInterval
      ( ..,
        StatusUpdateInterval_SECONDS_10,
        StatusUpdateInterval_SECONDS_12,
        StatusUpdateInterval_SECONDS_120,
        StatusUpdateInterval_SECONDS_15,
        StatusUpdateInterval_SECONDS_180,
        StatusUpdateInterval_SECONDS_20,
        StatusUpdateInterval_SECONDS_240,
        StatusUpdateInterval_SECONDS_30,
        StatusUpdateInterval_SECONDS_300,
        StatusUpdateInterval_SECONDS_360,
        StatusUpdateInterval_SECONDS_420,
        StatusUpdateInterval_SECONDS_480,
        StatusUpdateInterval_SECONDS_540,
        StatusUpdateInterval_SECONDS_60,
        StatusUpdateInterval_SECONDS_600
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon
-- CloudWatch Events. Set the interval, in seconds, between status updates.
-- MediaConvert sends an update at this interval from the time the service
-- begins processing your job to the time it completes the transcode or
-- encounters an error.
newtype StatusUpdateInterval = StatusUpdateInterval'
  { fromStatusUpdateInterval ::
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

pattern StatusUpdateInterval_SECONDS_10 :: StatusUpdateInterval
pattern StatusUpdateInterval_SECONDS_10 = StatusUpdateInterval' "SECONDS_10"

pattern StatusUpdateInterval_SECONDS_12 :: StatusUpdateInterval
pattern StatusUpdateInterval_SECONDS_12 = StatusUpdateInterval' "SECONDS_12"

pattern StatusUpdateInterval_SECONDS_120 :: StatusUpdateInterval
pattern StatusUpdateInterval_SECONDS_120 = StatusUpdateInterval' "SECONDS_120"

pattern StatusUpdateInterval_SECONDS_15 :: StatusUpdateInterval
pattern StatusUpdateInterval_SECONDS_15 = StatusUpdateInterval' "SECONDS_15"

pattern StatusUpdateInterval_SECONDS_180 :: StatusUpdateInterval
pattern StatusUpdateInterval_SECONDS_180 = StatusUpdateInterval' "SECONDS_180"

pattern StatusUpdateInterval_SECONDS_20 :: StatusUpdateInterval
pattern StatusUpdateInterval_SECONDS_20 = StatusUpdateInterval' "SECONDS_20"

pattern StatusUpdateInterval_SECONDS_240 :: StatusUpdateInterval
pattern StatusUpdateInterval_SECONDS_240 = StatusUpdateInterval' "SECONDS_240"

pattern StatusUpdateInterval_SECONDS_30 :: StatusUpdateInterval
pattern StatusUpdateInterval_SECONDS_30 = StatusUpdateInterval' "SECONDS_30"

pattern StatusUpdateInterval_SECONDS_300 :: StatusUpdateInterval
pattern StatusUpdateInterval_SECONDS_300 = StatusUpdateInterval' "SECONDS_300"

pattern StatusUpdateInterval_SECONDS_360 :: StatusUpdateInterval
pattern StatusUpdateInterval_SECONDS_360 = StatusUpdateInterval' "SECONDS_360"

pattern StatusUpdateInterval_SECONDS_420 :: StatusUpdateInterval
pattern StatusUpdateInterval_SECONDS_420 = StatusUpdateInterval' "SECONDS_420"

pattern StatusUpdateInterval_SECONDS_480 :: StatusUpdateInterval
pattern StatusUpdateInterval_SECONDS_480 = StatusUpdateInterval' "SECONDS_480"

pattern StatusUpdateInterval_SECONDS_540 :: StatusUpdateInterval
pattern StatusUpdateInterval_SECONDS_540 = StatusUpdateInterval' "SECONDS_540"

pattern StatusUpdateInterval_SECONDS_60 :: StatusUpdateInterval
pattern StatusUpdateInterval_SECONDS_60 = StatusUpdateInterval' "SECONDS_60"

pattern StatusUpdateInterval_SECONDS_600 :: StatusUpdateInterval
pattern StatusUpdateInterval_SECONDS_600 = StatusUpdateInterval' "SECONDS_600"

{-# COMPLETE
  StatusUpdateInterval_SECONDS_10,
  StatusUpdateInterval_SECONDS_12,
  StatusUpdateInterval_SECONDS_120,
  StatusUpdateInterval_SECONDS_15,
  StatusUpdateInterval_SECONDS_180,
  StatusUpdateInterval_SECONDS_20,
  StatusUpdateInterval_SECONDS_240,
  StatusUpdateInterval_SECONDS_30,
  StatusUpdateInterval_SECONDS_300,
  StatusUpdateInterval_SECONDS_360,
  StatusUpdateInterval_SECONDS_420,
  StatusUpdateInterval_SECONDS_480,
  StatusUpdateInterval_SECONDS_540,
  StatusUpdateInterval_SECONDS_60,
  StatusUpdateInterval_SECONDS_600,
  StatusUpdateInterval'
  #-}
