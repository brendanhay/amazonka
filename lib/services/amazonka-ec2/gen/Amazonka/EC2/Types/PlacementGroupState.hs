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
-- Module      : Amazonka.EC2.Types.PlacementGroupState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PlacementGroupState
  ( PlacementGroupState
      ( ..,
        PlacementGroupState_Available,
        PlacementGroupState_Deleted,
        PlacementGroupState_Deleting,
        PlacementGroupState_Pending
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype PlacementGroupState = PlacementGroupState'
  { fromPlacementGroupState ::
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

pattern PlacementGroupState_Available :: PlacementGroupState
pattern PlacementGroupState_Available = PlacementGroupState' "available"

pattern PlacementGroupState_Deleted :: PlacementGroupState
pattern PlacementGroupState_Deleted = PlacementGroupState' "deleted"

pattern PlacementGroupState_Deleting :: PlacementGroupState
pattern PlacementGroupState_Deleting = PlacementGroupState' "deleting"

pattern PlacementGroupState_Pending :: PlacementGroupState
pattern PlacementGroupState_Pending = PlacementGroupState' "pending"

{-# COMPLETE
  PlacementGroupState_Available,
  PlacementGroupState_Deleted,
  PlacementGroupState_Deleting,
  PlacementGroupState_Pending,
  PlacementGroupState'
  #-}
