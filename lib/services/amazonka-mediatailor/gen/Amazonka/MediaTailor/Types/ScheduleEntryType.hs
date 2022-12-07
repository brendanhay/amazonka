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
-- Module      : Amazonka.MediaTailor.Types.ScheduleEntryType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.ScheduleEntryType
  ( ScheduleEntryType
      ( ..,
        ScheduleEntryType_FILLER_SLATE,
        ScheduleEntryType_PROGRAM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScheduleEntryType = ScheduleEntryType'
  { fromScheduleEntryType ::
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

pattern ScheduleEntryType_FILLER_SLATE :: ScheduleEntryType
pattern ScheduleEntryType_FILLER_SLATE = ScheduleEntryType' "FILLER_SLATE"

pattern ScheduleEntryType_PROGRAM :: ScheduleEntryType
pattern ScheduleEntryType_PROGRAM = ScheduleEntryType' "PROGRAM"

{-# COMPLETE
  ScheduleEntryType_FILLER_SLATE,
  ScheduleEntryType_PROGRAM,
  ScheduleEntryType'
  #-}
