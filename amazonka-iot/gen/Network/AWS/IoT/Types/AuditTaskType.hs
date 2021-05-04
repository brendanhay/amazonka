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
-- Module      : Network.AWS.IoT.Types.AuditTaskType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditTaskType
  ( AuditTaskType
      ( ..,
        AuditTaskType_ON_DEMAND_AUDIT_TASK,
        AuditTaskType_SCHEDULED_AUDIT_TASK
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AuditTaskType = AuditTaskType'
  { fromAuditTaskType ::
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

pattern AuditTaskType_ON_DEMAND_AUDIT_TASK :: AuditTaskType
pattern AuditTaskType_ON_DEMAND_AUDIT_TASK = AuditTaskType' "ON_DEMAND_AUDIT_TASK"

pattern AuditTaskType_SCHEDULED_AUDIT_TASK :: AuditTaskType
pattern AuditTaskType_SCHEDULED_AUDIT_TASK = AuditTaskType' "SCHEDULED_AUDIT_TASK"

{-# COMPLETE
  AuditTaskType_ON_DEMAND_AUDIT_TASK,
  AuditTaskType_SCHEDULED_AUDIT_TASK,
  AuditTaskType'
  #-}
