{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype HsmStatus = HsmStatus'
  { fromHsmStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
