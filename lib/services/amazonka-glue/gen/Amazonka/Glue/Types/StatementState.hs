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
-- Module      : Amazonka.Glue.Types.StatementState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.StatementState
  ( StatementState
      ( ..,
        StatementState_AVAILABLE,
        StatementState_CANCELLED,
        StatementState_CANCELLING,
        StatementState_ERROR,
        StatementState_RUNNING,
        StatementState_WAITING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StatementState = StatementState'
  { fromStatementState ::
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

pattern StatementState_AVAILABLE :: StatementState
pattern StatementState_AVAILABLE = StatementState' "AVAILABLE"

pattern StatementState_CANCELLED :: StatementState
pattern StatementState_CANCELLED = StatementState' "CANCELLED"

pattern StatementState_CANCELLING :: StatementState
pattern StatementState_CANCELLING = StatementState' "CANCELLING"

pattern StatementState_ERROR :: StatementState
pattern StatementState_ERROR = StatementState' "ERROR"

pattern StatementState_RUNNING :: StatementState
pattern StatementState_RUNNING = StatementState' "RUNNING"

pattern StatementState_WAITING :: StatementState
pattern StatementState_WAITING = StatementState' "WAITING"

{-# COMPLETE
  StatementState_AVAILABLE,
  StatementState_CANCELLED,
  StatementState_CANCELLING,
  StatementState_ERROR,
  StatementState_RUNNING,
  StatementState_WAITING,
  StatementState'
  #-}
