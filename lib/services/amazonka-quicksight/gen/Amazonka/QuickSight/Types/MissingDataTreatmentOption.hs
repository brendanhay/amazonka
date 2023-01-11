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
-- Module      : Amazonka.QuickSight.Types.MissingDataTreatmentOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.MissingDataTreatmentOption
  ( MissingDataTreatmentOption
      ( ..,
        MissingDataTreatmentOption_INTERPOLATE,
        MissingDataTreatmentOption_SHOW_AS_BLANK,
        MissingDataTreatmentOption_SHOW_AS_ZERO
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MissingDataTreatmentOption = MissingDataTreatmentOption'
  { fromMissingDataTreatmentOption ::
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

pattern MissingDataTreatmentOption_INTERPOLATE :: MissingDataTreatmentOption
pattern MissingDataTreatmentOption_INTERPOLATE = MissingDataTreatmentOption' "INTERPOLATE"

pattern MissingDataTreatmentOption_SHOW_AS_BLANK :: MissingDataTreatmentOption
pattern MissingDataTreatmentOption_SHOW_AS_BLANK = MissingDataTreatmentOption' "SHOW_AS_BLANK"

pattern MissingDataTreatmentOption_SHOW_AS_ZERO :: MissingDataTreatmentOption
pattern MissingDataTreatmentOption_SHOW_AS_ZERO = MissingDataTreatmentOption' "SHOW_AS_ZERO"

{-# COMPLETE
  MissingDataTreatmentOption_INTERPOLATE,
  MissingDataTreatmentOption_SHOW_AS_BLANK,
  MissingDataTreatmentOption_SHOW_AS_ZERO,
  MissingDataTreatmentOption'
  #-}
