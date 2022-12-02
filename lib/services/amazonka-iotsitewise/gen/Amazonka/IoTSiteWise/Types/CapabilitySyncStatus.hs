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
-- Module      : Amazonka.IoTSiteWise.Types.CapabilitySyncStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.CapabilitySyncStatus
  ( CapabilitySyncStatus
      ( ..,
        CapabilitySyncStatus_IN_SYNC,
        CapabilitySyncStatus_OUT_OF_SYNC,
        CapabilitySyncStatus_SYNC_FAILED,
        CapabilitySyncStatus_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CapabilitySyncStatus = CapabilitySyncStatus'
  { fromCapabilitySyncStatus ::
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
