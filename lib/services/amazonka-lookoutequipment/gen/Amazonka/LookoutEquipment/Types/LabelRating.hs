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
-- Module      : Amazonka.LookoutEquipment.Types.LabelRating
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.LabelRating
  ( LabelRating
      ( ..,
        LabelRating_ANOMALY,
        LabelRating_NEUTRAL,
        LabelRating_NO_ANOMALY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LabelRating = LabelRating'
  { fromLabelRating ::
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

pattern LabelRating_ANOMALY :: LabelRating
pattern LabelRating_ANOMALY = LabelRating' "ANOMALY"

pattern LabelRating_NEUTRAL :: LabelRating
pattern LabelRating_NEUTRAL = LabelRating' "NEUTRAL"

pattern LabelRating_NO_ANOMALY :: LabelRating
pattern LabelRating_NO_ANOMALY = LabelRating' "NO_ANOMALY"

{-# COMPLETE
  LabelRating_ANOMALY,
  LabelRating_NEUTRAL,
  LabelRating_NO_ANOMALY,
  LabelRating'
  #-}
