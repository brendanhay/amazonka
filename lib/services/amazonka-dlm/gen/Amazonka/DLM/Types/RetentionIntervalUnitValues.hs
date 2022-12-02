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
-- Module      : Amazonka.DLM.Types.RetentionIntervalUnitValues
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.RetentionIntervalUnitValues
  ( RetentionIntervalUnitValues
      ( ..,
        RetentionIntervalUnitValues_DAYS,
        RetentionIntervalUnitValues_MONTHS,
        RetentionIntervalUnitValues_WEEKS,
        RetentionIntervalUnitValues_YEARS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RetentionIntervalUnitValues = RetentionIntervalUnitValues'
  { fromRetentionIntervalUnitValues ::
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

pattern RetentionIntervalUnitValues_DAYS :: RetentionIntervalUnitValues
pattern RetentionIntervalUnitValues_DAYS = RetentionIntervalUnitValues' "DAYS"

pattern RetentionIntervalUnitValues_MONTHS :: RetentionIntervalUnitValues
pattern RetentionIntervalUnitValues_MONTHS = RetentionIntervalUnitValues' "MONTHS"

pattern RetentionIntervalUnitValues_WEEKS :: RetentionIntervalUnitValues
pattern RetentionIntervalUnitValues_WEEKS = RetentionIntervalUnitValues' "WEEKS"

pattern RetentionIntervalUnitValues_YEARS :: RetentionIntervalUnitValues
pattern RetentionIntervalUnitValues_YEARS = RetentionIntervalUnitValues' "YEARS"

{-# COMPLETE
  RetentionIntervalUnitValues_DAYS,
  RetentionIntervalUnitValues_MONTHS,
  RetentionIntervalUnitValues_WEEKS,
  RetentionIntervalUnitValues_YEARS,
  RetentionIntervalUnitValues'
  #-}
