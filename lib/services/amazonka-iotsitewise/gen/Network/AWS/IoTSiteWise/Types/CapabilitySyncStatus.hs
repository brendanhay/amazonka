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
-- Module      : Network.AWS.IoTSiteWise.Types.CapabilitySyncStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTSiteWise.Types.CapabilitySyncStatus
  ( CapabilitySyncStatus
      ( ..,
        CapabilitySyncStatus_IN_SYNC,
        CapabilitySyncStatus_OUT_OF_SYNC,
        CapabilitySyncStatus_SYNC_FAILED,
        CapabilitySyncStatus_UNKNOWN
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype CapabilitySyncStatus = CapabilitySyncStatus'
  { fromCapabilitySyncStatus ::
      Core.Text
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

pattern CapabilitySyncStatus_IN_SYNC :: CapabilitySyncStatus
pattern CapabilitySyncStatus_IN_SYNC = CapabilitySyncStatus' "IN_SYNC"

pattern CapabilitySyncStatus_OUT_OF_SYNC :: CapabilitySyncStatus
pattern CapabilitySyncStatus_OUT_OF_SYNC = CapabilitySyncStatus' "OUT_OF_SYNC"

pattern CapabilitySyncStatus_SYNC_FAILED :: CapabilitySyncStatus
pattern CapabilitySyncStatus_SYNC_FAILED = CapabilitySyncStatus' "SYNC_FAILED"

pattern CapabilitySyncStatus_UNKNOWN :: CapabilitySyncStatus
pattern CapabilitySyncStatus_UNKNOWN = CapabilitySyncStatus' "UNKNOWN"

{-# COMPLETE
  CapabilitySyncStatus_IN_SYNC,
  CapabilitySyncStatus_OUT_OF_SYNC,
  CapabilitySyncStatus_SYNC_FAILED,
  CapabilitySyncStatus_UNKNOWN,
  CapabilitySyncStatus'
  #-}
