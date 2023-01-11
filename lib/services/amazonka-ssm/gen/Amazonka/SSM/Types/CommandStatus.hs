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
-- Module      : Amazonka.SSM.Types.CommandStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.CommandStatus
  ( CommandStatus
      ( ..,
        CommandStatus_Cancelled,
        CommandStatus_Cancelling,
        CommandStatus_Failed,
        CommandStatus_InProgress,
        CommandStatus_Pending,
        CommandStatus_Success,
        CommandStatus_TimedOut
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CommandStatus = CommandStatus'
  { fromCommandStatus ::
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

pattern CommandStatus_Cancelled :: CommandStatus
pattern CommandStatus_Cancelled = CommandStatus' "Cancelled"

pattern CommandStatus_Cancelling :: CommandStatus
pattern CommandStatus_Cancelling = CommandStatus' "Cancelling"

pattern CommandStatus_Failed :: CommandStatus
pattern CommandStatus_Failed = CommandStatus' "Failed"

pattern CommandStatus_InProgress :: CommandStatus
pattern CommandStatus_InProgress = CommandStatus' "InProgress"

pattern CommandStatus_Pending :: CommandStatus
pattern CommandStatus_Pending = CommandStatus' "Pending"

pattern CommandStatus_Success :: CommandStatus
pattern CommandStatus_Success = CommandStatus' "Success"

pattern CommandStatus_TimedOut :: CommandStatus
pattern CommandStatus_TimedOut = CommandStatus' "TimedOut"

{-# COMPLETE
  CommandStatus_Cancelled,
  CommandStatus_Cancelling,
  CommandStatus_Failed,
  CommandStatus_InProgress,
  CommandStatus_Pending,
  CommandStatus_Success,
  CommandStatus_TimedOut,
  CommandStatus'
  #-}
