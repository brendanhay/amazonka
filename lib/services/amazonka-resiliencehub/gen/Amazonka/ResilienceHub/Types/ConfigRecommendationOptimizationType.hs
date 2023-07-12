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
-- Module      : Amazonka.ResilienceHub.Types.ConfigRecommendationOptimizationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.ConfigRecommendationOptimizationType
  ( ConfigRecommendationOptimizationType
      ( ..,
        ConfigRecommendationOptimizationType_BestAZRecovery,
        ConfigRecommendationOptimizationType_BestAttainable,
        ConfigRecommendationOptimizationType_BestRegionRecovery,
        ConfigRecommendationOptimizationType_LeastChange,
        ConfigRecommendationOptimizationType_LeastCost,
        ConfigRecommendationOptimizationType_LeastErrors
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConfigRecommendationOptimizationType = ConfigRecommendationOptimizationType'
  { fromConfigRecommendationOptimizationType ::
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

pattern ConfigRecommendationOptimizationType_BestAZRecovery :: ConfigRecommendationOptimizationType
pattern ConfigRecommendationOptimizationType_BestAZRecovery = ConfigRecommendationOptimizationType' "BestAZRecovery"

pattern ConfigRecommendationOptimizationType_BestAttainable :: ConfigRecommendationOptimizationType
pattern ConfigRecommendationOptimizationType_BestAttainable = ConfigRecommendationOptimizationType' "BestAttainable"

pattern ConfigRecommendationOptimizationType_BestRegionRecovery :: ConfigRecommendationOptimizationType
pattern ConfigRecommendationOptimizationType_BestRegionRecovery = ConfigRecommendationOptimizationType' "BestRegionRecovery"

pattern ConfigRecommendationOptimizationType_LeastChange :: ConfigRecommendationOptimizationType
pattern ConfigRecommendationOptimizationType_LeastChange = ConfigRecommendationOptimizationType' "LeastChange"

pattern ConfigRecommendationOptimizationType_LeastCost :: ConfigRecommendationOptimizationType
pattern ConfigRecommendationOptimizationType_LeastCost = ConfigRecommendationOptimizationType' "LeastCost"

pattern ConfigRecommendationOptimizationType_LeastErrors :: ConfigRecommendationOptimizationType
pattern ConfigRecommendationOptimizationType_LeastErrors = ConfigRecommendationOptimizationType' "LeastErrors"

{-# COMPLETE
  ConfigRecommendationOptimizationType_BestAZRecovery,
  ConfigRecommendationOptimizationType_BestAttainable,
  ConfigRecommendationOptimizationType_BestRegionRecovery,
  ConfigRecommendationOptimizationType_LeastChange,
  ConfigRecommendationOptimizationType_LeastCost,
  ConfigRecommendationOptimizationType_LeastErrors,
  ConfigRecommendationOptimizationType'
  #-}
