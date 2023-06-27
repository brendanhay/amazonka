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
-- Module      : Amazonka.MediaConnect.Types.GatewayState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.GatewayState
  ( GatewayState
      ( ..,
        GatewayState_ACTIVE,
        GatewayState_CREATING,
        GatewayState_DELETED,
        GatewayState_DELETING,
        GatewayState_ERROR,
        GatewayState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GatewayState = GatewayState'
  { fromGatewayState ::
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

pattern GatewayState_ACTIVE :: GatewayState
pattern GatewayState_ACTIVE = GatewayState' "ACTIVE"

pattern GatewayState_CREATING :: GatewayState
pattern GatewayState_CREATING = GatewayState' "CREATING"

pattern GatewayState_DELETED :: GatewayState
pattern GatewayState_DELETED = GatewayState' "DELETED"

pattern GatewayState_DELETING :: GatewayState
pattern GatewayState_DELETING = GatewayState' "DELETING"

pattern GatewayState_ERROR :: GatewayState
pattern GatewayState_ERROR = GatewayState' "ERROR"

pattern GatewayState_UPDATING :: GatewayState
pattern GatewayState_UPDATING = GatewayState' "UPDATING"

{-# COMPLETE
  GatewayState_ACTIVE,
  GatewayState_CREATING,
  GatewayState_DELETED,
  GatewayState_DELETING,
  GatewayState_ERROR,
  GatewayState_UPDATING,
  GatewayState'
  #-}
