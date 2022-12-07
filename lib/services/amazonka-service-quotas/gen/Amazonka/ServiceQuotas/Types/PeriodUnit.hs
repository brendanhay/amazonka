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
-- Module      : Amazonka.ServiceQuotas.Types.PeriodUnit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceQuotas.Types.PeriodUnit
  ( PeriodUnit
      ( ..,
        PeriodUnit_DAY,
        PeriodUnit_HOUR,
        PeriodUnit_MICROSECOND,
        PeriodUnit_MILLISECOND,
        PeriodUnit_MINUTE,
        PeriodUnit_SECOND,
        PeriodUnit_WEEK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PeriodUnit = PeriodUnit'
  { fromPeriodUnit ::
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

pattern PeriodUnit_DAY :: PeriodUnit
pattern PeriodUnit_DAY = PeriodUnit' "DAY"

pattern PeriodUnit_HOUR :: PeriodUnit
pattern PeriodUnit_HOUR = PeriodUnit' "HOUR"

pattern PeriodUnit_MICROSECOND :: PeriodUnit
pattern PeriodUnit_MICROSECOND = PeriodUnit' "MICROSECOND"

pattern PeriodUnit_MILLISECOND :: PeriodUnit
pattern PeriodUnit_MILLISECOND = PeriodUnit' "MILLISECOND"

pattern PeriodUnit_MINUTE :: PeriodUnit
pattern PeriodUnit_MINUTE = PeriodUnit' "MINUTE"

pattern PeriodUnit_SECOND :: PeriodUnit
pattern PeriodUnit_SECOND = PeriodUnit' "SECOND"

pattern PeriodUnit_WEEK :: PeriodUnit
pattern PeriodUnit_WEEK = PeriodUnit' "WEEK"

{-# COMPLETE
  PeriodUnit_DAY,
  PeriodUnit_HOUR,
  PeriodUnit_MICROSECOND,
  PeriodUnit_MILLISECOND,
  PeriodUnit_MINUTE,
  PeriodUnit_SECOND,
  PeriodUnit_WEEK,
  PeriodUnit'
  #-}
