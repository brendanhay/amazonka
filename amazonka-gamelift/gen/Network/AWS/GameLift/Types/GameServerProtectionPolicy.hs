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
-- Module      : Network.AWS.GameLift.Types.GameServerProtectionPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerProtectionPolicy
  ( GameServerProtectionPolicy
      ( ..,
        GameServerProtectionPolicy_FULL_PROTECTION,
        GameServerProtectionPolicy_NO_PROTECTION
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype GameServerProtectionPolicy = GameServerProtectionPolicy'
  { fromGameServerProtectionPolicy ::
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

pattern GameServerProtectionPolicy_FULL_PROTECTION :: GameServerProtectionPolicy
pattern GameServerProtectionPolicy_FULL_PROTECTION = GameServerProtectionPolicy' "FULL_PROTECTION"

pattern GameServerProtectionPolicy_NO_PROTECTION :: GameServerProtectionPolicy
pattern GameServerProtectionPolicy_NO_PROTECTION = GameServerProtectionPolicy' "NO_PROTECTION"

{-# COMPLETE
  GameServerProtectionPolicy_FULL_PROTECTION,
  GameServerProtectionPolicy_NO_PROTECTION,
  GameServerProtectionPolicy'
  #-}
