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
-- Module      : Network.AWS.GameLift.Types.GameSessionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionStatus
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

import qualified Network.AWS.Core as Core

newtype GameSessionStatus = GameSessionStatus'
  { fromGameSessionStatus ::
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
