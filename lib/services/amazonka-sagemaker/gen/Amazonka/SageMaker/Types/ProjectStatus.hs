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
-- Module      : Amazonka.SageMaker.Types.ProjectStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProjectStatus
  ( ProjectStatus
      ( ..,
        ProjectStatus_CreateCompleted,
        ProjectStatus_CreateFailed,
        ProjectStatus_CreateInProgress,
        ProjectStatus_DeleteCompleted,
        ProjectStatus_DeleteFailed,
        ProjectStatus_DeleteInProgress,
        ProjectStatus_Pending,
        ProjectStatus_UpdateCompleted,
        ProjectStatus_UpdateFailed,
        ProjectStatus_UpdateInProgress
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProjectStatus = ProjectStatus'
  { fromProjectStatus ::
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

pattern ProjectStatus_CreateCompleted :: ProjectStatus
pattern ProjectStatus_CreateCompleted = ProjectStatus' "CreateCompleted"

pattern ProjectStatus_CreateFailed :: ProjectStatus
pattern ProjectStatus_CreateFailed = ProjectStatus' "CreateFailed"

pattern ProjectStatus_CreateInProgress :: ProjectStatus
pattern ProjectStatus_CreateInProgress = ProjectStatus' "CreateInProgress"

pattern ProjectStatus_DeleteCompleted :: ProjectStatus
pattern ProjectStatus_DeleteCompleted = ProjectStatus' "DeleteCompleted"

pattern ProjectStatus_DeleteFailed :: ProjectStatus
pattern ProjectStatus_DeleteFailed = ProjectStatus' "DeleteFailed"

pattern ProjectStatus_DeleteInProgress :: ProjectStatus
pattern ProjectStatus_DeleteInProgress = ProjectStatus' "DeleteInProgress"

pattern ProjectStatus_Pending :: ProjectStatus
pattern ProjectStatus_Pending = ProjectStatus' "Pending"

pattern ProjectStatus_UpdateCompleted :: ProjectStatus
pattern ProjectStatus_UpdateCompleted = ProjectStatus' "UpdateCompleted"

pattern ProjectStatus_UpdateFailed :: ProjectStatus
pattern ProjectStatus_UpdateFailed = ProjectStatus' "UpdateFailed"

pattern ProjectStatus_UpdateInProgress :: ProjectStatus
pattern ProjectStatus_UpdateInProgress = ProjectStatus' "UpdateInProgress"

{-# COMPLETE
  ProjectStatus_CreateCompleted,
  ProjectStatus_CreateFailed,
  ProjectStatus_CreateInProgress,
  ProjectStatus_DeleteCompleted,
  ProjectStatus_DeleteFailed,
  ProjectStatus_DeleteInProgress,
  ProjectStatus_Pending,
  ProjectStatus_UpdateCompleted,
  ProjectStatus_UpdateFailed,
  ProjectStatus_UpdateInProgress,
  ProjectStatus'
  #-}
