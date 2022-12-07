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
-- Module      : Amazonka.WorkMail.Types.MailboxExportJobState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.MailboxExportJobState
  ( MailboxExportJobState
      ( ..,
        MailboxExportJobState_CANCELLED,
        MailboxExportJobState_COMPLETED,
        MailboxExportJobState_FAILED,
        MailboxExportJobState_RUNNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MailboxExportJobState = MailboxExportJobState'
  { fromMailboxExportJobState ::
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
