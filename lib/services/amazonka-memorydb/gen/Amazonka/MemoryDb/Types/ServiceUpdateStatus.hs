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
-- Module      : Amazonka.MemoryDb.Types.ServiceUpdateStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.ServiceUpdateStatus
  ( ServiceUpdateStatus
      ( ..,
        ServiceUpdateStatus_Available,
        ServiceUpdateStatus_Complete,
        ServiceUpdateStatus_In_progress,
        ServiceUpdateStatus_Scheduled
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ServiceUpdateStatus = ServiceUpdateStatus'
  { fromServiceUpdateStatus ::
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

pattern ServiceUpdateStatus_Available :: ServiceUpdateStatus
pattern ServiceUpdateStatus_Available = ServiceUpdateStatus' "available"

pattern ServiceUpdateStatus_Complete :: ServiceUpdateStatus
pattern ServiceUpdateStatus_Complete = ServiceUpdateStatus' "complete"

pattern ServiceUpdateStatus_In_progress :: ServiceUpdateStatus
pattern ServiceUpdateStatus_In_progress = ServiceUpdateStatus' "in-progress"

pattern ServiceUpdateStatus_Scheduled :: ServiceUpdateStatus
pattern ServiceUpdateStatus_Scheduled = ServiceUpdateStatus' "scheduled"

{-# COMPLETE
  ServiceUpdateStatus_Available,
  ServiceUpdateStatus_Complete,
  ServiceUpdateStatus_In_progress,
  ServiceUpdateStatus_Scheduled,
  ServiceUpdateStatus'
  #-}
