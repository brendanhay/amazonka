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
-- Module      : Network.AWS.SageMaker.Types.ProjectStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProjectStatus
  ( ProjectStatus
      ( ..,
        ProjectStatus_CreateCompleted,
        ProjectStatus_CreateFailed,
        ProjectStatus_CreateInProgress,
        ProjectStatus_DeleteCompleted,
        ProjectStatus_DeleteFailed,
        ProjectStatus_DeleteInProgress,
        ProjectStatus_Pending
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ProjectStatus = ProjectStatus'
  { fromProjectStatus ::
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

{-# COMPLETE
  ProjectStatus_CreateCompleted,
  ProjectStatus_CreateFailed,
  ProjectStatus_CreateInProgress,
  ProjectStatus_DeleteCompleted,
  ProjectStatus_DeleteFailed,
  ProjectStatus_DeleteInProgress,
  ProjectStatus_Pending,
  ProjectStatus'
  #-}
