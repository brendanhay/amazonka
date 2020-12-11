-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.PlacementGroupStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.PlacementGroupStrategy
  ( PlacementGroupStrategy
      ( PlacementGroupStrategy',
        PGSCluster,
        PGSNone,
        PGSPartition,
        PGSSpread
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PlacementGroupStrategy = PlacementGroupStrategy' Lude.Text
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

pattern PGSCluster :: PlacementGroupStrategy
pattern PGSCluster = PlacementGroupStrategy' "CLUSTER"

pattern PGSNone :: PlacementGroupStrategy
pattern PGSNone = PlacementGroupStrategy' "NONE"

pattern PGSPartition :: PlacementGroupStrategy
pattern PGSPartition = PlacementGroupStrategy' "PARTITION"

pattern PGSSpread :: PlacementGroupStrategy
pattern PGSSpread = PlacementGroupStrategy' "SPREAD"

{-# COMPLETE
  PGSCluster,
  PGSNone,
  PGSPartition,
  PGSSpread,
  PlacementGroupStrategy'
  #-}
