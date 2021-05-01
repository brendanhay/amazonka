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
-- Module      : Network.AWS.IoT.Types.AuditMitigationActionsTaskStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditMitigationActionsTaskStatus
  ( AuditMitigationActionsTaskStatus
      ( ..,
        AuditMitigationActionsTaskStatus_CANCELED,
        AuditMitigationActionsTaskStatus_COMPLETED,
        AuditMitigationActionsTaskStatus_FAILED,
        AuditMitigationActionsTaskStatus_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AuditMitigationActionsTaskStatus = AuditMitigationActionsTaskStatus'
  { fromAuditMitigationActionsTaskStatus ::
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

pattern AuditMitigationActionsTaskStatus_CANCELED :: AuditMitigationActionsTaskStatus
pattern AuditMitigationActionsTaskStatus_CANCELED = AuditMitigationActionsTaskStatus' "CANCELED"

pattern AuditMitigationActionsTaskStatus_COMPLETED :: AuditMitigationActionsTaskStatus
pattern AuditMitigationActionsTaskStatus_COMPLETED = AuditMitigationActionsTaskStatus' "COMPLETED"

pattern AuditMitigationActionsTaskStatus_FAILED :: AuditMitigationActionsTaskStatus
pattern AuditMitigationActionsTaskStatus_FAILED = AuditMitigationActionsTaskStatus' "FAILED"

pattern AuditMitigationActionsTaskStatus_IN_PROGRESS :: AuditMitigationActionsTaskStatus
pattern AuditMitigationActionsTaskStatus_IN_PROGRESS = AuditMitigationActionsTaskStatus' "IN_PROGRESS"

{-# COMPLETE
  AuditMitigationActionsTaskStatus_CANCELED,
  AuditMitigationActionsTaskStatus_COMPLETED,
  AuditMitigationActionsTaskStatus_FAILED,
  AuditMitigationActionsTaskStatus_IN_PROGRESS,
  AuditMitigationActionsTaskStatus'
  #-}
