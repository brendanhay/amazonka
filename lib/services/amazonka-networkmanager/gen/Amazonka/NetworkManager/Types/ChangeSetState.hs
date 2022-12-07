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
-- Module      : Amazonka.NetworkManager.Types.ChangeSetState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.ChangeSetState
  ( ChangeSetState
      ( ..,
        ChangeSetState_EXECUTING,
        ChangeSetState_EXECUTION_SUCCEEDED,
        ChangeSetState_FAILED_GENERATION,
        ChangeSetState_OUT_OF_DATE,
        ChangeSetState_PENDING_GENERATION,
        ChangeSetState_READY_TO_EXECUTE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChangeSetState = ChangeSetState'
  { fromChangeSetState ::
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

pattern ChangeSetState_EXECUTING :: ChangeSetState
pattern ChangeSetState_EXECUTING = ChangeSetState' "EXECUTING"

pattern ChangeSetState_EXECUTION_SUCCEEDED :: ChangeSetState
pattern ChangeSetState_EXECUTION_SUCCEEDED = ChangeSetState' "EXECUTION_SUCCEEDED"

pattern ChangeSetState_FAILED_GENERATION :: ChangeSetState
pattern ChangeSetState_FAILED_GENERATION = ChangeSetState' "FAILED_GENERATION"

pattern ChangeSetState_OUT_OF_DATE :: ChangeSetState
pattern ChangeSetState_OUT_OF_DATE = ChangeSetState' "OUT_OF_DATE"

pattern ChangeSetState_PENDING_GENERATION :: ChangeSetState
pattern ChangeSetState_PENDING_GENERATION = ChangeSetState' "PENDING_GENERATION"

pattern ChangeSetState_READY_TO_EXECUTE :: ChangeSetState
pattern ChangeSetState_READY_TO_EXECUTE = ChangeSetState' "READY_TO_EXECUTE"

{-# COMPLETE
  ChangeSetState_EXECUTING,
  ChangeSetState_EXECUTION_SUCCEEDED,
  ChangeSetState_FAILED_GENERATION,
  ChangeSetState_OUT_OF_DATE,
  ChangeSetState_PENDING_GENERATION,
  ChangeSetState_READY_TO_EXECUTE,
  ChangeSetState'
  #-}
