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
-- Module      : Amazonka.SageMaker.Types.ModelCardProcessingStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelCardProcessingStatus
  ( ModelCardProcessingStatus
      ( ..,
        ModelCardProcessingStatus_ContentDeleted,
        ModelCardProcessingStatus_DeleteCompleted,
        ModelCardProcessingStatus_DeleteFailed,
        ModelCardProcessingStatus_DeleteInProgress,
        ModelCardProcessingStatus_DeletePending,
        ModelCardProcessingStatus_ExportJobsDeleted
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ModelCardProcessingStatus = ModelCardProcessingStatus'
  { fromModelCardProcessingStatus ::
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

pattern ModelCardProcessingStatus_ContentDeleted :: ModelCardProcessingStatus
pattern ModelCardProcessingStatus_ContentDeleted = ModelCardProcessingStatus' "ContentDeleted"

pattern ModelCardProcessingStatus_DeleteCompleted :: ModelCardProcessingStatus
pattern ModelCardProcessingStatus_DeleteCompleted = ModelCardProcessingStatus' "DeleteCompleted"

pattern ModelCardProcessingStatus_DeleteFailed :: ModelCardProcessingStatus
pattern ModelCardProcessingStatus_DeleteFailed = ModelCardProcessingStatus' "DeleteFailed"

pattern ModelCardProcessingStatus_DeleteInProgress :: ModelCardProcessingStatus
pattern ModelCardProcessingStatus_DeleteInProgress = ModelCardProcessingStatus' "DeleteInProgress"

pattern ModelCardProcessingStatus_DeletePending :: ModelCardProcessingStatus
pattern ModelCardProcessingStatus_DeletePending = ModelCardProcessingStatus' "DeletePending"

pattern ModelCardProcessingStatus_ExportJobsDeleted :: ModelCardProcessingStatus
pattern ModelCardProcessingStatus_ExportJobsDeleted = ModelCardProcessingStatus' "ExportJobsDeleted"

{-# COMPLETE
  ModelCardProcessingStatus_ContentDeleted,
  ModelCardProcessingStatus_DeleteCompleted,
  ModelCardProcessingStatus_DeleteFailed,
  ModelCardProcessingStatus_DeleteInProgress,
  ModelCardProcessingStatus_DeletePending,
  ModelCardProcessingStatus_ExportJobsDeleted,
  ModelCardProcessingStatus'
  #-}
