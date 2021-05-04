{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionStatus
  ( ActionExecutionStatus
      ( ..,
        ActionExecutionStatus_Abandoned,
        ActionExecutionStatus_Failed,
        ActionExecutionStatus_InProgress,
        ActionExecutionStatus_Succeeded
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ActionExecutionStatus = ActionExecutionStatus'
  { fromActionExecutionStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
