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
-- Module      : Amazonka.ECR.Types.ScanStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.ScanStatus
  ( ScanStatus
      ( ..,
        ScanStatus_ACTIVE,
        ScanStatus_COMPLETE,
        ScanStatus_FAILED,
        ScanStatus_FINDINGS_UNAVAILABLE,
        ScanStatus_IN_PROGRESS,
        ScanStatus_PENDING,
        ScanStatus_SCAN_ELIGIBILITY_EXPIRED,
        ScanStatus_UNSUPPORTED_IMAGE
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

pattern ScanStatus_ACTIVE :: ScanStatus
pattern ScanStatus_ACTIVE = ScanStatus' "ACTIVE"

pattern ScanStatus_COMPLETE :: ScanStatus
pattern ScanStatus_COMPLETE = ScanStatus' "COMPLETE"

pattern ScanStatus_FAILED :: ScanStatus
pattern ScanStatus_FAILED = ScanStatus' "FAILED"

pattern ScanStatus_FINDINGS_UNAVAILABLE :: ScanStatus
pattern ScanStatus_FINDINGS_UNAVAILABLE = ScanStatus' "FINDINGS_UNAVAILABLE"

pattern ScanStatus_IN_PROGRESS :: ScanStatus
pattern ScanStatus_IN_PROGRESS = ScanStatus' "IN_PROGRESS"

pattern ScanStatus_PENDING :: ScanStatus
pattern ScanStatus_PENDING = ScanStatus' "PENDING"

pattern ScanStatus_SCAN_ELIGIBILITY_EXPIRED :: ScanStatus
pattern ScanStatus_SCAN_ELIGIBILITY_EXPIRED = ScanStatus' "SCAN_ELIGIBILITY_EXPIRED"

pattern ScanStatus_UNSUPPORTED_IMAGE :: ScanStatus
pattern ScanStatus_UNSUPPORTED_IMAGE = ScanStatus' "UNSUPPORTED_IMAGE"

{-# COMPLETE
  ScanStatus_ACTIVE,
  ScanStatus_COMPLETE,
  ScanStatus_FAILED,
  ScanStatus_FINDINGS_UNAVAILABLE,
  ScanStatus_IN_PROGRESS,
  ScanStatus_PENDING,
  ScanStatus_SCAN_ELIGIBILITY_EXPIRED,
  ScanStatus_UNSUPPORTED_IMAGE,
  ScanStatus'
  #-}
