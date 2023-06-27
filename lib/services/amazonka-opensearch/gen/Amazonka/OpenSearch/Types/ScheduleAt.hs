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
-- Module      : Amazonka.OpenSearch.Types.ScheduleAt
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ScheduleAt
  ( ScheduleAt
      ( ..,
        ScheduleAt_NOW,
        ScheduleAt_OFF_PEAK_WINDOW,
        ScheduleAt_TIMESTAMP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScheduleAt = ScheduleAt'
  { fromScheduleAt ::
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

pattern ScheduleAt_NOW :: ScheduleAt
pattern ScheduleAt_NOW = ScheduleAt' "NOW"

pattern ScheduleAt_OFF_PEAK_WINDOW :: ScheduleAt
pattern ScheduleAt_OFF_PEAK_WINDOW = ScheduleAt' "OFF_PEAK_WINDOW"

pattern ScheduleAt_TIMESTAMP :: ScheduleAt
pattern ScheduleAt_TIMESTAMP = ScheduleAt' "TIMESTAMP"

{-# COMPLETE
  ScheduleAt_NOW,
  ScheduleAt_OFF_PEAK_WINDOW,
  ScheduleAt_TIMESTAMP,
  ScheduleAt'
  #-}
