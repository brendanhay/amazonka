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
-- Module      : Network.AWS.EMR.Types.PlacementGroupStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.PlacementGroupStrategy
  ( PlacementGroupStrategy
      ( ..,
        PlacementGroupStrategy_CLUSTER,
        PlacementGroupStrategy_NONE,
        PlacementGroupStrategy_PARTITION,
        PlacementGroupStrategy_SPREAD
      ),
  )
where

import qualified Network.AWS.Core as Core

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
