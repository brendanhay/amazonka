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
-- Module      : Amazonka.EC2.Types.PlacementStrategy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PlacementStrategy
  ( PlacementStrategy
      ( ..,
        PlacementStrategy_Cluster,
        PlacementStrategy_Partition,
        PlacementStrategy_Spread
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype PlacementStrategy = PlacementStrategy'
  { fromPlacementStrategy ::
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

pattern PlacementStrategy_Cluster :: PlacementStrategy
pattern PlacementStrategy_Cluster = PlacementStrategy' "cluster"

pattern PlacementStrategy_Partition :: PlacementStrategy
pattern PlacementStrategy_Partition = PlacementStrategy' "partition"

pattern PlacementStrategy_Spread :: PlacementStrategy
pattern PlacementStrategy_Spread = PlacementStrategy' "spread"

{-# COMPLETE
  PlacementStrategy_Cluster,
  PlacementStrategy_Partition,
  PlacementStrategy_Spread,
  PlacementStrategy'
  #-}
