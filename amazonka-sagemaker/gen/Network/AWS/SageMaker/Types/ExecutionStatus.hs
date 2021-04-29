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
-- Module      : Network.AWS.SageMaker.Types.ExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ExecutionStatus
  ( ExecutionStatus
      ( ..,
        ExecutionStatus_Completed,
        ExecutionStatus_CompletedWithViolations,
        ExecutionStatus_Failed,
        ExecutionStatus_InProgress,
        ExecutionStatus_Pending,
        ExecutionStatus_Stopped,
        ExecutionStatus_Stopping
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ExecutionStatus = ExecutionStatus'
  { fromExecutionStatus ::
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

pattern ExecutionStatus_Completed :: ExecutionStatus
pattern ExecutionStatus_Completed = ExecutionStatus' "Completed"

pattern ExecutionStatus_CompletedWithViolations :: ExecutionStatus
pattern ExecutionStatus_CompletedWithViolations = ExecutionStatus' "CompletedWithViolations"

pattern ExecutionStatus_Failed :: ExecutionStatus
pattern ExecutionStatus_Failed = ExecutionStatus' "Failed"

pattern ExecutionStatus_InProgress :: ExecutionStatus
pattern ExecutionStatus_InProgress = ExecutionStatus' "InProgress"

pattern ExecutionStatus_Pending :: ExecutionStatus
pattern ExecutionStatus_Pending = ExecutionStatus' "Pending"

pattern ExecutionStatus_Stopped :: ExecutionStatus
pattern ExecutionStatus_Stopped = ExecutionStatus' "Stopped"

pattern ExecutionStatus_Stopping :: ExecutionStatus
pattern ExecutionStatus_Stopping = ExecutionStatus' "Stopping"

{-# COMPLETE
  ExecutionStatus_Completed,
  ExecutionStatus_CompletedWithViolations,
  ExecutionStatus_Failed,
  ExecutionStatus_InProgress,
  ExecutionStatus_Pending,
  ExecutionStatus_Stopped,
  ExecutionStatus_Stopping,
  ExecutionStatus'
  #-}
