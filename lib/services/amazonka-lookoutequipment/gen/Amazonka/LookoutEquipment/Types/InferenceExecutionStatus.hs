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
-- Module      : Amazonka.LookoutEquipment.Types.InferenceExecutionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.InferenceExecutionStatus
  ( InferenceExecutionStatus
      ( ..,
        InferenceExecutionStatus_FAILED,
        InferenceExecutionStatus_IN_PROGRESS,
        InferenceExecutionStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InferenceExecutionStatus = InferenceExecutionStatus'
  { fromInferenceExecutionStatus ::
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

pattern InferenceExecutionStatus_FAILED :: InferenceExecutionStatus
pattern InferenceExecutionStatus_FAILED = InferenceExecutionStatus' "FAILED"

pattern InferenceExecutionStatus_IN_PROGRESS :: InferenceExecutionStatus
pattern InferenceExecutionStatus_IN_PROGRESS = InferenceExecutionStatus' "IN_PROGRESS"

pattern InferenceExecutionStatus_SUCCESS :: InferenceExecutionStatus
pattern InferenceExecutionStatus_SUCCESS = InferenceExecutionStatus' "SUCCESS"

{-# COMPLETE
  InferenceExecutionStatus_FAILED,
  InferenceExecutionStatus_IN_PROGRESS,
  InferenceExecutionStatus_SUCCESS,
  InferenceExecutionStatus'
  #-}
