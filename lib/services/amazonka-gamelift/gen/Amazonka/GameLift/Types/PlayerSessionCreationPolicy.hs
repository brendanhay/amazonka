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
-- Module      : Amazonka.GameLift.Types.PlayerSessionCreationPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.PlayerSessionCreationPolicy
  ( PlayerSessionCreationPolicy
      ( ..,
        PlayerSessionCreationPolicy_ACCEPT_ALL,
        PlayerSessionCreationPolicy_DENY_ALL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PlayerSessionCreationPolicy = PlayerSessionCreationPolicy'
  { fromPlayerSessionCreationPolicy ::
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

pattern PlayerSessionCreationPolicy_ACCEPT_ALL :: PlayerSessionCreationPolicy
pattern PlayerSessionCreationPolicy_ACCEPT_ALL = PlayerSessionCreationPolicy' "ACCEPT_ALL"

pattern PlayerSessionCreationPolicy_DENY_ALL :: PlayerSessionCreationPolicy
pattern PlayerSessionCreationPolicy_DENY_ALL = PlayerSessionCreationPolicy' "DENY_ALL"

{-# COMPLETE
  PlayerSessionCreationPolicy_ACCEPT_ALL,
  PlayerSessionCreationPolicy_DENY_ALL,
  PlayerSessionCreationPolicy'
  #-}
