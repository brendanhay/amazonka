{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.PlacementStrategyType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.PlacementStrategyType
  ( PlacementStrategyType
      ( ..,
        PlacementStrategyType_Binpack,
        PlacementStrategyType_Random,
        PlacementStrategyType_Spread
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype PlacementStrategyType = PlacementStrategyType'
  { fromPlacementStrategyType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
