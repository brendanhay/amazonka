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
-- Module      : Network.AWS.CloudHSM.Types.HsmStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSM.Types.HsmStatus
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

import qualified Network.AWS.Core as Core

newtype HsmStatus = HsmStatus'
  { fromHsmStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
