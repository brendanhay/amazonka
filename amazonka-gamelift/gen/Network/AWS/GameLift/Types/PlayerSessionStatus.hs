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
-- Module      : Network.AWS.GameLift.Types.PlayerSessionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.PlayerSessionStatus
  ( PlayerSessionStatus
      ( ..,
        PlayerSessionStatus_ACTIVE,
        PlayerSessionStatus_COMPLETED,
        PlayerSessionStatus_RESERVED,
        PlayerSessionStatus_TIMEDOUT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype PlayerSessionStatus = PlayerSessionStatus'
  { fromPlayerSessionStatus ::
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

pattern PlayerSessionStatus_ACTIVE :: PlayerSessionStatus
pattern PlayerSessionStatus_ACTIVE = PlayerSessionStatus' "ACTIVE"

pattern PlayerSessionStatus_COMPLETED :: PlayerSessionStatus
pattern PlayerSessionStatus_COMPLETED = PlayerSessionStatus' "COMPLETED"

pattern PlayerSessionStatus_RESERVED :: PlayerSessionStatus
pattern PlayerSessionStatus_RESERVED = PlayerSessionStatus' "RESERVED"

pattern PlayerSessionStatus_TIMEDOUT :: PlayerSessionStatus
pattern PlayerSessionStatus_TIMEDOUT = PlayerSessionStatus' "TIMEDOUT"

{-# COMPLETE
  PlayerSessionStatus_ACTIVE,
  PlayerSessionStatus_COMPLETED,
  PlayerSessionStatus_RESERVED,
  PlayerSessionStatus_TIMEDOUT,
  PlayerSessionStatus'
  #-}
