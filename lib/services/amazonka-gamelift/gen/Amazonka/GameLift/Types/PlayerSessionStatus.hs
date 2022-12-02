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
-- Module      : Amazonka.GameLift.Types.PlayerSessionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.PlayerSessionStatus
  ( PlayerSessionStatus
      ( ..,
        PlayerSessionStatus_ACTIVE,
        PlayerSessionStatus_COMPLETED,
        PlayerSessionStatus_RESERVED,
        PlayerSessionStatus_TIMEDOUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PlayerSessionStatus = PlayerSessionStatus'
  { fromPlayerSessionStatus ::
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
