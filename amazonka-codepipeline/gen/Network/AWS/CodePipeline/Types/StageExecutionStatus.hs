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
-- Module      : Network.AWS.CodePipeline.Types.StageExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StageExecutionStatus
  ( StageExecutionStatus
      ( ..,
        StageExecutionStatus_Cancelled,
        StageExecutionStatus_Failed,
        StageExecutionStatus_InProgress,
        StageExecutionStatus_Stopped,
        StageExecutionStatus_Stopping,
        StageExecutionStatus_Succeeded
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype StageExecutionStatus = StageExecutionStatus'
  { fromStageExecutionStatus ::
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

pattern StageExecutionStatus_Cancelled :: StageExecutionStatus
pattern StageExecutionStatus_Cancelled = StageExecutionStatus' "Cancelled"

pattern StageExecutionStatus_Failed :: StageExecutionStatus
pattern StageExecutionStatus_Failed = StageExecutionStatus' "Failed"

pattern StageExecutionStatus_InProgress :: StageExecutionStatus
pattern StageExecutionStatus_InProgress = StageExecutionStatus' "InProgress"

pattern StageExecutionStatus_Stopped :: StageExecutionStatus
pattern StageExecutionStatus_Stopped = StageExecutionStatus' "Stopped"

pattern StageExecutionStatus_Stopping :: StageExecutionStatus
pattern StageExecutionStatus_Stopping = StageExecutionStatus' "Stopping"

pattern StageExecutionStatus_Succeeded :: StageExecutionStatus
pattern StageExecutionStatus_Succeeded = StageExecutionStatus' "Succeeded"

{-# COMPLETE
  StageExecutionStatus_Cancelled,
  StageExecutionStatus_Failed,
  StageExecutionStatus_InProgress,
  StageExecutionStatus_Stopped,
  StageExecutionStatus_Stopping,
  StageExecutionStatus_Succeeded,
  StageExecutionStatus'
  #-}
