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
-- Module      : Network.AWS.EC2.Types.PlacementGroupStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PlacementGroupStrategy
  ( PlacementGroupStrategy
      ( ..,
        PlacementGroupStrategy_Cluster,
        PlacementGroupStrategy_Partition,
        PlacementGroupStrategy_Spread
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype PlacementGroupStrategy = PlacementGroupStrategy'
  { fromPlacementGroupStrategy ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern PlacementGroupStrategy_Cluster :: PlacementGroupStrategy
pattern PlacementGroupStrategy_Cluster = PlacementGroupStrategy' "cluster"

pattern PlacementGroupStrategy_Partition :: PlacementGroupStrategy
pattern PlacementGroupStrategy_Partition = PlacementGroupStrategy' "partition"

pattern PlacementGroupStrategy_Spread :: PlacementGroupStrategy
pattern PlacementGroupStrategy_Spread = PlacementGroupStrategy' "spread"

{-# COMPLETE
  PlacementGroupStrategy_Cluster,
  PlacementGroupStrategy_Partition,
  PlacementGroupStrategy_Spread,
  PlacementGroupStrategy'
  #-}
