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
-- Module      : Amazonka.DataSync.Types.DiscoveryJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.DiscoveryJobStatus
  ( DiscoveryJobStatus
      ( ..,
        DiscoveryJobStatus_COMPLETED,
        DiscoveryJobStatus_COMPLETED_WITH_ISSUES,
        DiscoveryJobStatus_FAILED,
        DiscoveryJobStatus_RUNNING,
        DiscoveryJobStatus_STOPPED,
        DiscoveryJobStatus_TERMINATED,
        DiscoveryJobStatus_WARNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DiscoveryJobStatus = DiscoveryJobStatus'
  { fromDiscoveryJobStatus ::
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

pattern DiscoveryJobStatus_COMPLETED :: DiscoveryJobStatus
pattern DiscoveryJobStatus_COMPLETED = DiscoveryJobStatus' "COMPLETED"

pattern DiscoveryJobStatus_COMPLETED_WITH_ISSUES :: DiscoveryJobStatus
pattern DiscoveryJobStatus_COMPLETED_WITH_ISSUES = DiscoveryJobStatus' "COMPLETED_WITH_ISSUES"

pattern DiscoveryJobStatus_FAILED :: DiscoveryJobStatus
pattern DiscoveryJobStatus_FAILED = DiscoveryJobStatus' "FAILED"

pattern DiscoveryJobStatus_RUNNING :: DiscoveryJobStatus
pattern DiscoveryJobStatus_RUNNING = DiscoveryJobStatus' "RUNNING"

pattern DiscoveryJobStatus_STOPPED :: DiscoveryJobStatus
pattern DiscoveryJobStatus_STOPPED = DiscoveryJobStatus' "STOPPED"

pattern DiscoveryJobStatus_TERMINATED :: DiscoveryJobStatus
pattern DiscoveryJobStatus_TERMINATED = DiscoveryJobStatus' "TERMINATED"

pattern DiscoveryJobStatus_WARNING :: DiscoveryJobStatus
pattern DiscoveryJobStatus_WARNING = DiscoveryJobStatus' "WARNING"

{-# COMPLETE
  DiscoveryJobStatus_COMPLETED,
  DiscoveryJobStatus_COMPLETED_WITH_ISSUES,
  DiscoveryJobStatus_FAILED,
  DiscoveryJobStatus_RUNNING,
  DiscoveryJobStatus_STOPPED,
  DiscoveryJobStatus_TERMINATED,
  DiscoveryJobStatus_WARNING,
  DiscoveryJobStatus'
  #-}
