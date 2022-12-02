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
-- Module      : Amazonka.Config.Types.RemediationExecutionState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.RemediationExecutionState
  ( RemediationExecutionState
      ( ..,
        RemediationExecutionState_FAILED,
        RemediationExecutionState_IN_PROGRESS,
        RemediationExecutionState_QUEUED,
        RemediationExecutionState_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RemediationExecutionState = RemediationExecutionState'
  { fromRemediationExecutionState ::
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

pattern RemediationExecutionState_FAILED :: RemediationExecutionState
pattern RemediationExecutionState_FAILED = RemediationExecutionState' "FAILED"

pattern RemediationExecutionState_IN_PROGRESS :: RemediationExecutionState
pattern RemediationExecutionState_IN_PROGRESS = RemediationExecutionState' "IN_PROGRESS"

pattern RemediationExecutionState_QUEUED :: RemediationExecutionState
pattern RemediationExecutionState_QUEUED = RemediationExecutionState' "QUEUED"

pattern RemediationExecutionState_SUCCEEDED :: RemediationExecutionState
pattern RemediationExecutionState_SUCCEEDED = RemediationExecutionState' "SUCCEEDED"

{-# COMPLETE
  RemediationExecutionState_FAILED,
  RemediationExecutionState_IN_PROGRESS,
  RemediationExecutionState_QUEUED,
  RemediationExecutionState_SUCCEEDED,
  RemediationExecutionState'
  #-}
