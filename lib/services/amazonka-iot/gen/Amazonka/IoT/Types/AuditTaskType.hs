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
-- Module      : Amazonka.IoT.Types.AuditTaskType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuditTaskType
  ( AuditTaskType
      ( ..,
        AuditTaskType_ON_DEMAND_AUDIT_TASK,
        AuditTaskType_SCHEDULED_AUDIT_TASK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AuditTaskType = AuditTaskType'
  { fromAuditTaskType ::
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

pattern AuditTaskType_ON_DEMAND_AUDIT_TASK :: AuditTaskType
pattern AuditTaskType_ON_DEMAND_AUDIT_TASK = AuditTaskType' "ON_DEMAND_AUDIT_TASK"

pattern AuditTaskType_SCHEDULED_AUDIT_TASK :: AuditTaskType
pattern AuditTaskType_SCHEDULED_AUDIT_TASK = AuditTaskType' "SCHEDULED_AUDIT_TASK"

{-# COMPLETE
  AuditTaskType_ON_DEMAND_AUDIT_TASK,
  AuditTaskType_SCHEDULED_AUDIT_TASK,
  AuditTaskType'
  #-}
