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
-- Module      : Amazonka.Redshift.Types.UsageLimitPeriod
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.UsageLimitPeriod
  ( UsageLimitPeriod
      ( ..,
        UsageLimitPeriod_Daily,
        UsageLimitPeriod_Monthly,
        UsageLimitPeriod_Weekly
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

newtype UsageLimitPeriod = UsageLimitPeriod'
  { fromUsageLimitPeriod ::
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
