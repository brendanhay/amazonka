-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.MailboxExportJobState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.MailboxExportJobState
  ( MailboxExportJobState
      ( MailboxExportJobState',
        Cancelled,
        Completed,
        Failed,
        Running
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MailboxExportJobState = MailboxExportJobState' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Cancelled :: MailboxExportJobState
pattern Cancelled = MailboxExportJobState' "CANCELLED"

pattern Completed :: MailboxExportJobState
pattern Completed = MailboxExportJobState' "COMPLETED"

pattern Failed :: MailboxExportJobState
pattern Failed = MailboxExportJobState' "FAILED"

pattern Running :: MailboxExportJobState
pattern Running = MailboxExportJobState' "RUNNING"

{-# COMPLETE
  Cancelled,
  Completed,
  Failed,
  Running,
  MailboxExportJobState'
  #-}
