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
-- Module      : Amazonka.GameLift.Types.GameServerInstanceStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.GameServerInstanceStatus
  ( GameServerInstanceStatus
      ( ..,
        GameServerInstanceStatus_ACTIVE,
        GameServerInstanceStatus_DRAINING,
        GameServerInstanceStatus_SPOT_TERMINATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GameServerInstanceStatus = GameServerInstanceStatus'
  { fromGameServerInstanceStatus ::
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

pattern GameServerInstanceStatus_ACTIVE :: GameServerInstanceStatus
pattern GameServerInstanceStatus_ACTIVE = GameServerInstanceStatus' "ACTIVE"

pattern GameServerInstanceStatus_DRAINING :: GameServerInstanceStatus
pattern GameServerInstanceStatus_DRAINING = GameServerInstanceStatus' "DRAINING"

pattern GameServerInstanceStatus_SPOT_TERMINATING :: GameServerInstanceStatus
pattern GameServerInstanceStatus_SPOT_TERMINATING = GameServerInstanceStatus' "SPOT_TERMINATING"

{-# COMPLETE
  GameServerInstanceStatus_ACTIVE,
  GameServerInstanceStatus_DRAINING,
  GameServerInstanceStatus_SPOT_TERMINATING,
  GameServerInstanceStatus'
  #-}
