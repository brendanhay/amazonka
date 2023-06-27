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
-- Module      : Amazonka.VPCLattice.Types.TargetStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.TargetStatus
  ( TargetStatus
      ( ..,
        TargetStatus_DRAINING,
        TargetStatus_HEALTHY,
        TargetStatus_INITIAL,
        TargetStatus_UNAVAILABLE,
        TargetStatus_UNHEALTHY,
        TargetStatus_UNUSED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TargetStatus = TargetStatus'
  { fromTargetStatus ::
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

pattern TargetStatus_DRAINING :: TargetStatus
pattern TargetStatus_DRAINING = TargetStatus' "DRAINING"

pattern TargetStatus_HEALTHY :: TargetStatus
pattern TargetStatus_HEALTHY = TargetStatus' "HEALTHY"

pattern TargetStatus_INITIAL :: TargetStatus
pattern TargetStatus_INITIAL = TargetStatus' "INITIAL"

pattern TargetStatus_UNAVAILABLE :: TargetStatus
pattern TargetStatus_UNAVAILABLE = TargetStatus' "UNAVAILABLE"

pattern TargetStatus_UNHEALTHY :: TargetStatus
pattern TargetStatus_UNHEALTHY = TargetStatus' "UNHEALTHY"

pattern TargetStatus_UNUSED :: TargetStatus
pattern TargetStatus_UNUSED = TargetStatus' "UNUSED"

{-# COMPLETE
  TargetStatus_DRAINING,
  TargetStatus_HEALTHY,
  TargetStatus_INITIAL,
  TargetStatus_UNAVAILABLE,
  TargetStatus_UNHEALTHY,
  TargetStatus_UNUSED,
  TargetStatus'
  #-}
