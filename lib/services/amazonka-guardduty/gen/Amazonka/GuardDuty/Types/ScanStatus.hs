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
-- Module      : Amazonka.GuardDuty.Types.ScanStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ScanStatus
  ( ScanStatus
      ( ..,
        ScanStatus_COMPLETED,
        ScanStatus_FAILED,
        ScanStatus_RUNNING,
        ScanStatus_SKIPPED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScanStatus = ScanStatus'
  { fromScanStatus ::
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

pattern ScanStatus_COMPLETED :: ScanStatus
pattern ScanStatus_COMPLETED = ScanStatus' "COMPLETED"

pattern ScanStatus_FAILED :: ScanStatus
pattern ScanStatus_FAILED = ScanStatus' "FAILED"

pattern ScanStatus_RUNNING :: ScanStatus
pattern ScanStatus_RUNNING = ScanStatus' "RUNNING"

pattern ScanStatus_SKIPPED :: ScanStatus
pattern ScanStatus_SKIPPED = ScanStatus' "SKIPPED"

{-# COMPLETE
  ScanStatus_COMPLETED,
  ScanStatus_FAILED,
  ScanStatus_RUNNING,
  ScanStatus_SKIPPED,
  ScanStatus'
  #-}
