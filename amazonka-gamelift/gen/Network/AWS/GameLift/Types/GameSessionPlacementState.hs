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
-- Module      : Network.AWS.GameLift.Types.GameSessionPlacementState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionPlacementState
  ( GameSessionPlacementState
      ( ..,
        GameSessionPlacementState_CANCELLED,
        GameSessionPlacementState_FAILED,
        GameSessionPlacementState_FULFILLED,
        GameSessionPlacementState_PENDING,
        GameSessionPlacementState_TIMED_OUT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype GameSessionPlacementState = GameSessionPlacementState'
  { fromGameSessionPlacementState ::
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

pattern GameSessionPlacementState_CANCELLED :: GameSessionPlacementState
pattern GameSessionPlacementState_CANCELLED = GameSessionPlacementState' "CANCELLED"

pattern GameSessionPlacementState_FAILED :: GameSessionPlacementState
pattern GameSessionPlacementState_FAILED = GameSessionPlacementState' "FAILED"

pattern GameSessionPlacementState_FULFILLED :: GameSessionPlacementState
pattern GameSessionPlacementState_FULFILLED = GameSessionPlacementState' "FULFILLED"

pattern GameSessionPlacementState_PENDING :: GameSessionPlacementState
pattern GameSessionPlacementState_PENDING = GameSessionPlacementState' "PENDING"

pattern GameSessionPlacementState_TIMED_OUT :: GameSessionPlacementState
pattern GameSessionPlacementState_TIMED_OUT = GameSessionPlacementState' "TIMED_OUT"

{-# COMPLETE
  GameSessionPlacementState_CANCELLED,
  GameSessionPlacementState_FAILED,
  GameSessionPlacementState_FULFILLED,
  GameSessionPlacementState_PENDING,
  GameSessionPlacementState_TIMED_OUT,
  GameSessionPlacementState'
  #-}
