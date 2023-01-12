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
-- Module      : Amazonka.QuickSight.Types.ForecastComputationSeasonality
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ForecastComputationSeasonality
  ( ForecastComputationSeasonality
      ( ..,
        ForecastComputationSeasonality_AUTOMATIC,
        ForecastComputationSeasonality_CUSTOM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ForecastComputationSeasonality = ForecastComputationSeasonality'
  { fromForecastComputationSeasonality ::
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

pattern ForecastComputationSeasonality_AUTOMATIC :: ForecastComputationSeasonality
pattern ForecastComputationSeasonality_AUTOMATIC = ForecastComputationSeasonality' "AUTOMATIC"

pattern ForecastComputationSeasonality_CUSTOM :: ForecastComputationSeasonality
pattern ForecastComputationSeasonality_CUSTOM = ForecastComputationSeasonality' "CUSTOM"

{-# COMPLETE
  ForecastComputationSeasonality_AUTOMATIC,
  ForecastComputationSeasonality_CUSTOM,
  ForecastComputationSeasonality'
  #-}
