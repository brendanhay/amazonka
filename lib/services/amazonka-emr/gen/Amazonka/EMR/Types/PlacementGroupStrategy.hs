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
-- Module      : Amazonka.EMR.Types.PlacementGroupStrategy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.PlacementGroupStrategy
  ( PlacementGroupStrategy
      ( ..,
        PlacementGroupStrategy_CLUSTER,
        PlacementGroupStrategy_NONE,
        PlacementGroupStrategy_PARTITION,
        PlacementGroupStrategy_SPREAD
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PlacementGroupStrategy = PlacementGroupStrategy'
  { fromPlacementGroupStrategy ::
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

pattern PlacementGroupStrategy_CLUSTER :: PlacementGroupStrategy
pattern PlacementGroupStrategy_CLUSTER = PlacementGroupStrategy' "CLUSTER"

pattern PlacementGroupStrategy_NONE :: PlacementGroupStrategy
pattern PlacementGroupStrategy_NONE = PlacementGroupStrategy' "NONE"

pattern PlacementGroupStrategy_PARTITION :: PlacementGroupStrategy
pattern PlacementGroupStrategy_PARTITION = PlacementGroupStrategy' "PARTITION"

pattern PlacementGroupStrategy_SPREAD :: PlacementGroupStrategy
pattern PlacementGroupStrategy_SPREAD = PlacementGroupStrategy' "SPREAD"

{-# COMPLETE
  PlacementGroupStrategy_CLUSTER,
  PlacementGroupStrategy_NONE,
  PlacementGroupStrategy_PARTITION,
  PlacementGroupStrategy_SPREAD,
  PlacementGroupStrategy'
  #-}
