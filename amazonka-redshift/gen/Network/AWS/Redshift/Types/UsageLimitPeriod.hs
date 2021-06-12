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
-- Module      : Network.AWS.Redshift.Types.UsageLimitPeriod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.UsageLimitPeriod
  ( UsageLimitPeriod
      ( ..,
        UsageLimitPeriod_Daily,
        UsageLimitPeriod_Monthly,
        UsageLimitPeriod_Weekly
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Redshift.Internal

newtype UsageLimitPeriod = UsageLimitPeriod'
  { fromUsageLimitPeriod ::
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

pattern UsageLimitPeriod_Daily :: UsageLimitPeriod
pattern UsageLimitPeriod_Daily = UsageLimitPeriod' "daily"

pattern UsageLimitPeriod_Monthly :: UsageLimitPeriod
pattern UsageLimitPeriod_Monthly = UsageLimitPeriod' "monthly"

pattern UsageLimitPeriod_Weekly :: UsageLimitPeriod
pattern UsageLimitPeriod_Weekly = UsageLimitPeriod' "weekly"

{-# COMPLETE
  UsageLimitPeriod_Daily,
  UsageLimitPeriod_Monthly,
  UsageLimitPeriod_Weekly,
  UsageLimitPeriod'
  #-}
