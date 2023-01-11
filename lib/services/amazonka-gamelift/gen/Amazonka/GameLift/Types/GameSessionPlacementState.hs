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
-- Module      : Amazonka.GameLift.Types.GameSessionPlacementState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.GameSessionPlacementState
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GameSessionPlacementState = GameSessionPlacementState'
  { fromGameSessionPlacementState ::
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
