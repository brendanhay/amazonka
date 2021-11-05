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
-- Module      : Amazonka.SSM.Types.StepExecutionFilterKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.StepExecutionFilterKey
  ( StepExecutionFilterKey
      ( ..,
        StepExecutionFilterKey_Action,
        StepExecutionFilterKey_StartTimeAfter,
        StepExecutionFilterKey_StartTimeBefore,
        StepExecutionFilterKey_StepExecutionId,
        StepExecutionFilterKey_StepExecutionStatus,
        StepExecutionFilterKey_StepName
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype StepExecutionFilterKey = StepExecutionFilterKey'
  { fromStepExecutionFilterKey ::
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

pattern StepExecutionFilterKey_Action :: StepExecutionFilterKey
pattern StepExecutionFilterKey_Action = StepExecutionFilterKey' "Action"

pattern StepExecutionFilterKey_StartTimeAfter :: StepExecutionFilterKey
pattern StepExecutionFilterKey_StartTimeAfter = StepExecutionFilterKey' "StartTimeAfter"

pattern StepExecutionFilterKey_StartTimeBefore :: StepExecutionFilterKey
pattern StepExecutionFilterKey_StartTimeBefore = StepExecutionFilterKey' "StartTimeBefore"

pattern StepExecutionFilterKey_StepExecutionId :: StepExecutionFilterKey
pattern StepExecutionFilterKey_StepExecutionId = StepExecutionFilterKey' "StepExecutionId"

pattern StepExecutionFilterKey_StepExecutionStatus :: StepExecutionFilterKey
pattern StepExecutionFilterKey_StepExecutionStatus = StepExecutionFilterKey' "StepExecutionStatus"

pattern StepExecutionFilterKey_StepName :: StepExecutionFilterKey
pattern StepExecutionFilterKey_StepName = StepExecutionFilterKey' "StepName"

{-# COMPLETE
  StepExecutionFilterKey_Action,
  StepExecutionFilterKey_StartTimeAfter,
  StepExecutionFilterKey_StartTimeBefore,
  StepExecutionFilterKey_StepExecutionId,
  StepExecutionFilterKey_StepExecutionStatus,
  StepExecutionFilterKey_StepName,
  StepExecutionFilterKey'
  #-}
