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
-- Module      : Amazonka.EKS.Types.AddonStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.AddonStatus
  ( AddonStatus
      ( ..,
        AddonStatus_ACTIVE,
        AddonStatus_CREATE_FAILED,
        AddonStatus_CREATING,
        AddonStatus_DEGRADED,
        AddonStatus_DELETE_FAILED,
        AddonStatus_DELETING,
        AddonStatus_UPDATE_FAILED,
        AddonStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AddonStatus = AddonStatus'
  { fromAddonStatus ::
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

pattern AddonStatus_ACTIVE :: AddonStatus
pattern AddonStatus_ACTIVE = AddonStatus' "ACTIVE"

pattern AddonStatus_CREATE_FAILED :: AddonStatus
pattern AddonStatus_CREATE_FAILED = AddonStatus' "CREATE_FAILED"

pattern AddonStatus_CREATING :: AddonStatus
pattern AddonStatus_CREATING = AddonStatus' "CREATING"

pattern AddonStatus_DEGRADED :: AddonStatus
pattern AddonStatus_DEGRADED = AddonStatus' "DEGRADED"

pattern AddonStatus_DELETE_FAILED :: AddonStatus
pattern AddonStatus_DELETE_FAILED = AddonStatus' "DELETE_FAILED"

pattern AddonStatus_DELETING :: AddonStatus
pattern AddonStatus_DELETING = AddonStatus' "DELETING"

pattern AddonStatus_UPDATE_FAILED :: AddonStatus
pattern AddonStatus_UPDATE_FAILED = AddonStatus' "UPDATE_FAILED"

pattern AddonStatus_UPDATING :: AddonStatus
pattern AddonStatus_UPDATING = AddonStatus' "UPDATING"

{-# COMPLETE
  AddonStatus_ACTIVE,
  AddonStatus_CREATE_FAILED,
  AddonStatus_CREATING,
  AddonStatus_DEGRADED,
  AddonStatus_DELETE_FAILED,
  AddonStatus_DELETING,
  AddonStatus_UPDATE_FAILED,
  AddonStatus_UPDATING,
  AddonStatus'
  #-}
