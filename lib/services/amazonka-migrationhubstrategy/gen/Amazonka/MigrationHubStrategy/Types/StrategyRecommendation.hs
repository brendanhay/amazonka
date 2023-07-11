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
-- Module      : Amazonka.MigrationHubStrategy.Types.StrategyRecommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.StrategyRecommendation
  ( StrategyRecommendation
      ( ..,
        StrategyRecommendation_NotRecommended,
        StrategyRecommendation_Potential,
        StrategyRecommendation_Recommended,
        StrategyRecommendation_ViableOption
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StrategyRecommendation = StrategyRecommendation'
  { fromStrategyRecommendation ::
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

pattern StrategyRecommendation_NotRecommended :: StrategyRecommendation
pattern StrategyRecommendation_NotRecommended = StrategyRecommendation' "notRecommended"

pattern StrategyRecommendation_Potential :: StrategyRecommendation
pattern StrategyRecommendation_Potential = StrategyRecommendation' "potential"

pattern StrategyRecommendation_Recommended :: StrategyRecommendation
pattern StrategyRecommendation_Recommended = StrategyRecommendation' "recommended"

pattern StrategyRecommendation_ViableOption :: StrategyRecommendation
pattern StrategyRecommendation_ViableOption = StrategyRecommendation' "viableOption"

{-# COMPLETE
  StrategyRecommendation_NotRecommended,
  StrategyRecommendation_Potential,
  StrategyRecommendation_Recommended,
  StrategyRecommendation_ViableOption,
  StrategyRecommendation'
  #-}
