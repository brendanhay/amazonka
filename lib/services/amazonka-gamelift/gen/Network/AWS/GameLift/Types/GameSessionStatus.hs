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
-- Module      : Amazonka.GameLift.Types.GameSessionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.GameSessionStatus
  ( GameSessionStatus
      ( ..,
        GameSessionStatus_ACTIVATING,
        GameSessionStatus_ACTIVE,
        GameSessionStatus_ERROR,
        GameSessionStatus_TERMINATED,
        GameSessionStatus_TERMINATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype GameSessionStatus = GameSessionStatus'
  { fromGameSessionStatus ::
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

pattern GameSessionStatus_ACTIVATING :: GameSessionStatus
pattern GameSessionStatus_ACTIVATING = GameSessionStatus' "ACTIVATING"

pattern GameSessionStatus_ACTIVE :: GameSessionStatus
pattern GameSessionStatus_ACTIVE = GameSessionStatus' "ACTIVE"

pattern GameSessionStatus_ERROR :: GameSessionStatus
pattern GameSessionStatus_ERROR = GameSessionStatus' "ERROR"

pattern GameSessionStatus_TERMINATED :: GameSessionStatus
pattern GameSessionStatus_TERMINATED = GameSessionStatus' "TERMINATED"

pattern GameSessionStatus_TERMINATING :: GameSessionStatus
pattern GameSessionStatus_TERMINATING = GameSessionStatus' "TERMINATING"

{-# COMPLETE
  GameSessionStatus_ACTIVATING,
  GameSessionStatus_ACTIVE,
  GameSessionStatus_ERROR,
  GameSessionStatus_TERMINATED,
  GameSessionStatus_TERMINATING,
  GameSessionStatus'
  #-}
