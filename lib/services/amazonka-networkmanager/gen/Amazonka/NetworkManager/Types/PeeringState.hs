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
-- Module      : Amazonka.NetworkManager.Types.PeeringState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.PeeringState
  ( PeeringState
      ( ..,
        PeeringState_AVAILABLE,
        PeeringState_CREATING,
        PeeringState_DELETING,
        PeeringState_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PeeringState = PeeringState'
  { fromPeeringState ::
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

pattern PeeringState_AVAILABLE :: PeeringState
pattern PeeringState_AVAILABLE = PeeringState' "AVAILABLE"

pattern PeeringState_CREATING :: PeeringState
pattern PeeringState_CREATING = PeeringState' "CREATING"

pattern PeeringState_DELETING :: PeeringState
pattern PeeringState_DELETING = PeeringState' "DELETING"

pattern PeeringState_FAILED :: PeeringState
pattern PeeringState_FAILED = PeeringState' "FAILED"

{-# COMPLETE
  PeeringState_AVAILABLE,
  PeeringState_CREATING,
  PeeringState_DELETING,
  PeeringState_FAILED,
  PeeringState'
  #-}
