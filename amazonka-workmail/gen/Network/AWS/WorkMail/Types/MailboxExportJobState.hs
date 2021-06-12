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
-- Module      : Network.AWS.WorkMail.Types.MailboxExportJobState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.MailboxExportJobState
  ( MailboxExportJobState
      ( ..,
        MailboxExportJobState_CANCELLED,
        MailboxExportJobState_COMPLETED,
        MailboxExportJobState_FAILED,
        MailboxExportJobState_RUNNING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MailboxExportJobState = MailboxExportJobState'
  { fromMailboxExportJobState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern MailboxExportJobState_CANCELLED :: MailboxExportJobState
pattern MailboxExportJobState_CANCELLED = MailboxExportJobState' "CANCELLED"

pattern MailboxExportJobState_COMPLETED :: MailboxExportJobState
pattern MailboxExportJobState_COMPLETED = MailboxExportJobState' "COMPLETED"

pattern MailboxExportJobState_FAILED :: MailboxExportJobState
pattern MailboxExportJobState_FAILED = MailboxExportJobState' "FAILED"

pattern MailboxExportJobState_RUNNING :: MailboxExportJobState
pattern MailboxExportJobState_RUNNING = MailboxExportJobState' "RUNNING"

{-# COMPLETE
  MailboxExportJobState_CANCELLED,
  MailboxExportJobState_COMPLETED,
  MailboxExportJobState_FAILED,
  MailboxExportJobState_RUNNING,
  MailboxExportJobState'
  #-}
