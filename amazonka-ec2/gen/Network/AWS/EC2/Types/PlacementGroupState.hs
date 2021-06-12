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
-- Module      : Network.AWS.EC2.Types.PlacementGroupState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PlacementGroupState
  ( PlacementGroupState
      ( ..,
        PlacementGroupState_Available,
        PlacementGroupState_Deleted,
        PlacementGroupState_Deleting,
        PlacementGroupState_Pending
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype PlacementGroupState = PlacementGroupState'
  { fromPlacementGroupState ::
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
