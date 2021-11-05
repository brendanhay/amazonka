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
-- Module      : Network.AWS.DLM.Types.RetentionIntervalUnitValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DLM.Types.RetentionIntervalUnitValues
  ( RetentionIntervalUnitValues
      ( ..,
        RetentionIntervalUnitValues_DAYS,
        RetentionIntervalUnitValues_MONTHS,
        RetentionIntervalUnitValues_WEEKS,
        RetentionIntervalUnitValues_YEARS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype RetentionIntervalUnitValues = RetentionIntervalUnitValues'
  { fromRetentionIntervalUnitValues ::
      Core.Text
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
