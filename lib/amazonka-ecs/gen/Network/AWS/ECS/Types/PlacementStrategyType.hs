-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.PlacementStrategyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.PlacementStrategyType
  ( PlacementStrategyType
      ( PlacementStrategyType',
        Binpack,
        Random,
        Spread
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PlacementStrategyType = PlacementStrategyType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Binpack :: PlacementStrategyType
pattern Binpack = PlacementStrategyType' "binpack"

pattern Random :: PlacementStrategyType
pattern Random = PlacementStrategyType' "random"

pattern Spread :: PlacementStrategyType
pattern Spread = PlacementStrategyType' "spread"

{-# COMPLETE
  Binpack,
  Random,
  Spread,
  PlacementStrategyType'
  #-}
