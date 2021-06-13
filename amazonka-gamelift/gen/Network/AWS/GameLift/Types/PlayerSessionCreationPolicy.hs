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
-- Module      : Network.AWS.GameLift.Types.PlayerSessionCreationPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.PlayerSessionCreationPolicy
  ( PlayerSessionCreationPolicy
      ( ..,
        PlayerSessionCreationPolicy_ACCEPT_ALL,
        PlayerSessionCreationPolicy_DENY_ALL
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PlayerSessionCreationPolicy = PlayerSessionCreationPolicy'
  { fromPlayerSessionCreationPolicy ::
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

pattern PlayerSessionCreationPolicy_ACCEPT_ALL :: PlayerSessionCreationPolicy
pattern PlayerSessionCreationPolicy_ACCEPT_ALL = PlayerSessionCreationPolicy' "ACCEPT_ALL"

pattern PlayerSessionCreationPolicy_DENY_ALL :: PlayerSessionCreationPolicy
pattern PlayerSessionCreationPolicy_DENY_ALL = PlayerSessionCreationPolicy' "DENY_ALL"

{-# COMPLETE
  PlayerSessionCreationPolicy_ACCEPT_ALL,
  PlayerSessionCreationPolicy_DENY_ALL,
  PlayerSessionCreationPolicy'
  #-}
