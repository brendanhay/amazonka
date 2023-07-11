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
-- Module      : Amazonka.MacieV2.Types.TimeRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.TimeRange
  ( TimeRange
      ( ..,
        TimeRange_MONTH_TO_DATE,
        TimeRange_PAST_30_DAYS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An inclusive time period that Amazon Macie usage data applies to.
-- Possible values are:
newtype TimeRange = TimeRange'
  { fromTimeRange ::
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

pattern TimeRange_MONTH_TO_DATE :: TimeRange
pattern TimeRange_MONTH_TO_DATE = TimeRange' "MONTH_TO_DATE"

pattern TimeRange_PAST_30_DAYS :: TimeRange
pattern TimeRange_PAST_30_DAYS = TimeRange' "PAST_30_DAYS"

{-# COMPLETE
  TimeRange_MONTH_TO_DATE,
  TimeRange_PAST_30_DAYS,
  TimeRange'
  #-}
