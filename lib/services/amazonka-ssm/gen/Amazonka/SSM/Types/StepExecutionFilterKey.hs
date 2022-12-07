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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StepExecutionFilterKey = StepExecutionFilterKey'
  { fromStepExecutionFilterKey ::
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
