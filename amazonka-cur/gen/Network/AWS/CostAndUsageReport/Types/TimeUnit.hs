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
-- Module      : Network.AWS.CostAndUsageReport.Types.TimeUnit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types.TimeUnit
  ( TimeUnit
      ( ..,
        TimeUnit_DAILY,
        TimeUnit_HOURLY,
        TimeUnit_MONTHLY
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The length of time covered by the report.
newtype TimeUnit = TimeUnit'
  { fromTimeUnit ::
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

pattern TimeUnit_DAILY :: TimeUnit
pattern TimeUnit_DAILY = TimeUnit' "DAILY"

pattern TimeUnit_HOURLY :: TimeUnit
pattern TimeUnit_HOURLY = TimeUnit' "HOURLY"

pattern TimeUnit_MONTHLY :: TimeUnit
pattern TimeUnit_MONTHLY = TimeUnit' "MONTHLY"

{-# COMPLETE
  TimeUnit_DAILY,
  TimeUnit_HOURLY,
  TimeUnit_MONTHLY,
  TimeUnit'
  #-}
