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
-- Module      : Amazonka.EC2.Types.PlacementGroupStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PlacementGroupStrategy
  ( PlacementGroupStrategy
      ( ..,
        PlacementGroupStrategy_Cluster,
        PlacementGroupStrategy_Partition,
        PlacementGroupStrategy_Spread
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype PlacementGroupStrategy = PlacementGroupStrategy'
  { fromPlacementGroupStrategy ::
      Core.Text
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
