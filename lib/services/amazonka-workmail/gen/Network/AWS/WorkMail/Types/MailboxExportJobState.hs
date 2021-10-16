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
import qualified Network.AWS.Prelude as Prelude

newtype MailboxExportJobState = MailboxExportJobState'
  { fromMailboxExportJobState ::
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
