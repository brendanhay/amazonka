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
-- Module      : Amazonka.Config.Types.RemediationExecutionStepState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.RemediationExecutionStepState
  ( RemediationExecutionStepState
      ( ..,
        RemediationExecutionStepState_FAILED,
        RemediationExecutionStepState_PENDING,
        RemediationExecutionStepState_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RemediationExecutionStepState = RemediationExecutionStepState'
  { fromRemediationExecutionStepState ::
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

pattern RemediationExecutionStepState_FAILED :: RemediationExecutionStepState
pattern RemediationExecutionStepState_FAILED = RemediationExecutionStepState' "FAILED"

pattern RemediationExecutionStepState_PENDING :: RemediationExecutionStepState
pattern RemediationExecutionStepState_PENDING = RemediationExecutionStepState' "PENDING"

pattern RemediationExecutionStepState_SUCCEEDED :: RemediationExecutionStepState
pattern RemediationExecutionStepState_SUCCEEDED = RemediationExecutionStepState' "SUCCEEDED"

{-# COMPLETE
  RemediationExecutionStepState_FAILED,
  RemediationExecutionStepState_PENDING,
  RemediationExecutionStepState_SUCCEEDED,
  RemediationExecutionStepState'
  #-}
