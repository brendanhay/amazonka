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
-- Module      : Amazonka.ECS.Types.PlacementStrategyType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.PlacementStrategyType
  ( PlacementStrategyType
      ( ..,
        PlacementStrategyType_Binpack,
        PlacementStrategyType_Random,
        PlacementStrategyType_Spread
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PlacementStrategyType = PlacementStrategyType'
  { fromPlacementStrategyType ::
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

pattern PlacementStrategyType_Binpack :: PlacementStrategyType
pattern PlacementStrategyType_Binpack = PlacementStrategyType' "binpack"

pattern PlacementStrategyType_Random :: PlacementStrategyType
pattern PlacementStrategyType_Random = PlacementStrategyType' "random"

pattern PlacementStrategyType_Spread :: PlacementStrategyType
pattern PlacementStrategyType_Spread = PlacementStrategyType' "spread"

{-# COMPLETE
  PlacementStrategyType_Binpack,
  PlacementStrategyType_Random,
  PlacementStrategyType_Spread,
  PlacementStrategyType'
  #-}
