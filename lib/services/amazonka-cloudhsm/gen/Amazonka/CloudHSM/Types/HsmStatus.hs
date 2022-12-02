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
-- Module      : Amazonka.CloudHSM.Types.HsmStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudHSM.Types.HsmStatus
  ( HsmStatus
      ( ..,
        HsmStatus_DEGRADED,
        HsmStatus_PENDING,
        HsmStatus_RUNNING,
        HsmStatus_SUSPENDED,
        HsmStatus_TERMINATED,
        HsmStatus_TERMINATING,
        HsmStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HsmStatus = HsmStatus'
  { fromHsmStatus ::
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

pattern HsmStatus_DEGRADED :: HsmStatus
pattern HsmStatus_DEGRADED = HsmStatus' "DEGRADED"

pattern HsmStatus_PENDING :: HsmStatus
pattern HsmStatus_PENDING = HsmStatus' "PENDING"

pattern HsmStatus_RUNNING :: HsmStatus
pattern HsmStatus_RUNNING = HsmStatus' "RUNNING"

pattern HsmStatus_SUSPENDED :: HsmStatus
pattern HsmStatus_SUSPENDED = HsmStatus' "SUSPENDED"

pattern HsmStatus_TERMINATED :: HsmStatus
pattern HsmStatus_TERMINATED = HsmStatus' "TERMINATED"

pattern HsmStatus_TERMINATING :: HsmStatus
pattern HsmStatus_TERMINATING = HsmStatus' "TERMINATING"

pattern HsmStatus_UPDATING :: HsmStatus
pattern HsmStatus_UPDATING = HsmStatus' "UPDATING"

{-# COMPLETE
  HsmStatus_DEGRADED,
  HsmStatus_PENDING,
  HsmStatus_RUNNING,
  HsmStatus_SUSPENDED,
  HsmStatus_TERMINATED,
  HsmStatus_TERMINATING,
  HsmStatus_UPDATING,
  HsmStatus'
  #-}
