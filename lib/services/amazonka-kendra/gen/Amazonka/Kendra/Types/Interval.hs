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
-- Module      : Amazonka.Kendra.Types.Interval
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.Interval
  ( Interval
      ( ..,
        Interval_ONE_MONTH_AGO,
        Interval_ONE_WEEK_AGO,
        Interval_THIS_MONTH,
        Interval_THIS_WEEK,
        Interval_TWO_MONTHS_AGO,
        Interval_TWO_WEEKS_AGO
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Interval = Interval'
  { fromInterval ::
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

pattern Interval_ONE_MONTH_AGO :: Interval
pattern Interval_ONE_MONTH_AGO = Interval' "ONE_MONTH_AGO"

pattern Interval_ONE_WEEK_AGO :: Interval
pattern Interval_ONE_WEEK_AGO = Interval' "ONE_WEEK_AGO"

pattern Interval_THIS_MONTH :: Interval
pattern Interval_THIS_MONTH = Interval' "THIS_MONTH"

pattern Interval_THIS_WEEK :: Interval
pattern Interval_THIS_WEEK = Interval' "THIS_WEEK"

pattern Interval_TWO_MONTHS_AGO :: Interval
pattern Interval_TWO_MONTHS_AGO = Interval' "TWO_MONTHS_AGO"

pattern Interval_TWO_WEEKS_AGO :: Interval
pattern Interval_TWO_WEEKS_AGO = Interval' "TWO_WEEKS_AGO"

{-# COMPLETE
  Interval_ONE_MONTH_AGO,
  Interval_ONE_WEEK_AGO,
  Interval_THIS_MONTH,
  Interval_THIS_WEEK,
  Interval_TWO_MONTHS_AGO,
  Interval_TWO_WEEKS_AGO,
  Interval'
  #-}
