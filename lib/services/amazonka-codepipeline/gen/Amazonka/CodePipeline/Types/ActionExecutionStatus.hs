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
-- Module      : Amazonka.CodePipeline.Types.ActionExecutionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionExecutionStatus
  ( ActionExecutionStatus
      ( ..,
        ActionExecutionStatus_Abandoned,
        ActionExecutionStatus_Failed,
        ActionExecutionStatus_InProgress,
        ActionExecutionStatus_Succeeded
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActionExecutionStatus = ActionExecutionStatus'
  { fromActionExecutionStatus ::
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

pattern ActionExecutionStatus_Abandoned :: ActionExecutionStatus
pattern ActionExecutionStatus_Abandoned = ActionExecutionStatus' "Abandoned"

pattern ActionExecutionStatus_Failed :: ActionExecutionStatus
pattern ActionExecutionStatus_Failed = ActionExecutionStatus' "Failed"

pattern ActionExecutionStatus_InProgress :: ActionExecutionStatus
pattern ActionExecutionStatus_InProgress = ActionExecutionStatus' "InProgress"

pattern ActionExecutionStatus_Succeeded :: ActionExecutionStatus
pattern ActionExecutionStatus_Succeeded = ActionExecutionStatus' "Succeeded"

{-# COMPLETE
  ActionExecutionStatus_Abandoned,
  ActionExecutionStatus_Failed,
  ActionExecutionStatus_InProgress,
  ActionExecutionStatus_Succeeded,
  ActionExecutionStatus'
  #-}
