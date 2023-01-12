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
-- Module      : Amazonka.GameLift.Types.GameServerProtectionPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.GameServerProtectionPolicy
  ( GameServerProtectionPolicy
      ( ..,
        GameServerProtectionPolicy_FULL_PROTECTION,
        GameServerProtectionPolicy_NO_PROTECTION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GameServerProtectionPolicy = GameServerProtectionPolicy'
  { fromGameServerProtectionPolicy ::
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

pattern GameServerProtectionPolicy_FULL_PROTECTION :: GameServerProtectionPolicy
pattern GameServerProtectionPolicy_FULL_PROTECTION = GameServerProtectionPolicy' "FULL_PROTECTION"

pattern GameServerProtectionPolicy_NO_PROTECTION :: GameServerProtectionPolicy
pattern GameServerProtectionPolicy_NO_PROTECTION = GameServerProtectionPolicy' "NO_PROTECTION"

{-# COMPLETE
  GameServerProtectionPolicy_FULL_PROTECTION,
  GameServerProtectionPolicy_NO_PROTECTION,
  GameServerProtectionPolicy'
  #-}
