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
-- Module      : Amazonka.IoT.Types.AuditCheckRunStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuditCheckRunStatus
  ( AuditCheckRunStatus
      ( ..,
        AuditCheckRunStatus_CANCELED,
        AuditCheckRunStatus_COMPLETED_COMPLIANT,
        AuditCheckRunStatus_COMPLETED_NON_COMPLIANT,
        AuditCheckRunStatus_FAILED,
        AuditCheckRunStatus_IN_PROGRESS,
        AuditCheckRunStatus_WAITING_FOR_DATA_COLLECTION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AuditCheckRunStatus = AuditCheckRunStatus'
  { fromAuditCheckRunStatus ::
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

pattern AuditCheckRunStatus_CANCELED :: AuditCheckRunStatus
pattern AuditCheckRunStatus_CANCELED = AuditCheckRunStatus' "CANCELED"

pattern AuditCheckRunStatus_COMPLETED_COMPLIANT :: AuditCheckRunStatus
pattern AuditCheckRunStatus_COMPLETED_COMPLIANT = AuditCheckRunStatus' "COMPLETED_COMPLIANT"

pattern AuditCheckRunStatus_COMPLETED_NON_COMPLIANT :: AuditCheckRunStatus
pattern AuditCheckRunStatus_COMPLETED_NON_COMPLIANT = AuditCheckRunStatus' "COMPLETED_NON_COMPLIANT"

pattern AuditCheckRunStatus_FAILED :: AuditCheckRunStatus
pattern AuditCheckRunStatus_FAILED = AuditCheckRunStatus' "FAILED"

pattern AuditCheckRunStatus_IN_PROGRESS :: AuditCheckRunStatus
pattern AuditCheckRunStatus_IN_PROGRESS = AuditCheckRunStatus' "IN_PROGRESS"

pattern AuditCheckRunStatus_WAITING_FOR_DATA_COLLECTION :: AuditCheckRunStatus
pattern AuditCheckRunStatus_WAITING_FOR_DATA_COLLECTION = AuditCheckRunStatus' "WAITING_FOR_DATA_COLLECTION"

{-# COMPLETE
  AuditCheckRunStatus_CANCELED,
  AuditCheckRunStatus_COMPLETED_COMPLIANT,
  AuditCheckRunStatus_COMPLETED_NON_COMPLIANT,
  AuditCheckRunStatus_FAILED,
  AuditCheckRunStatus_IN_PROGRESS,
  AuditCheckRunStatus_WAITING_FOR_DATA_COLLECTION,
  AuditCheckRunStatus'
  #-}
