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
-- Module      : Network.AWS.IoT.Types.AuditTaskStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditTaskStatus
  ( AuditTaskStatus
      ( ..,
        AuditTaskStatus_CANCELED,
        AuditTaskStatus_COMPLETED,
        AuditTaskStatus_FAILED,
        AuditTaskStatus_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AuditTaskStatus = AuditTaskStatus'
  { fromAuditTaskStatus ::
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

pattern AuditTaskStatus_CANCELED :: AuditTaskStatus
pattern AuditTaskStatus_CANCELED = AuditTaskStatus' "CANCELED"

pattern AuditTaskStatus_COMPLETED :: AuditTaskStatus
pattern AuditTaskStatus_COMPLETED = AuditTaskStatus' "COMPLETED"

pattern AuditTaskStatus_FAILED :: AuditTaskStatus
pattern AuditTaskStatus_FAILED = AuditTaskStatus' "FAILED"

pattern AuditTaskStatus_IN_PROGRESS :: AuditTaskStatus
pattern AuditTaskStatus_IN_PROGRESS = AuditTaskStatus' "IN_PROGRESS"

{-# COMPLETE
  AuditTaskStatus_CANCELED,
  AuditTaskStatus_COMPLETED,
  AuditTaskStatus_FAILED,
  AuditTaskStatus_IN_PROGRESS,
  AuditTaskStatus'
  #-}
