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
-- Module      : Amazonka.SageMaker.Types.ModelPackageGroupStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelPackageGroupStatus
  ( ModelPackageGroupStatus
      ( ..,
        ModelPackageGroupStatus_Completed,
        ModelPackageGroupStatus_DeleteFailed,
        ModelPackageGroupStatus_Deleting,
        ModelPackageGroupStatus_Failed,
        ModelPackageGroupStatus_InProgress,
        ModelPackageGroupStatus_Pending
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ModelPackageGroupStatus = ModelPackageGroupStatus'
  { fromModelPackageGroupStatus ::
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

pattern ModelPackageGroupStatus_Completed :: ModelPackageGroupStatus
pattern ModelPackageGroupStatus_Completed = ModelPackageGroupStatus' "Completed"

pattern ModelPackageGroupStatus_DeleteFailed :: ModelPackageGroupStatus
pattern ModelPackageGroupStatus_DeleteFailed = ModelPackageGroupStatus' "DeleteFailed"

pattern ModelPackageGroupStatus_Deleting :: ModelPackageGroupStatus
pattern ModelPackageGroupStatus_Deleting = ModelPackageGroupStatus' "Deleting"

pattern ModelPackageGroupStatus_Failed :: ModelPackageGroupStatus
pattern ModelPackageGroupStatus_Failed = ModelPackageGroupStatus' "Failed"

pattern ModelPackageGroupStatus_InProgress :: ModelPackageGroupStatus
pattern ModelPackageGroupStatus_InProgress = ModelPackageGroupStatus' "InProgress"

pattern ModelPackageGroupStatus_Pending :: ModelPackageGroupStatus
pattern ModelPackageGroupStatus_Pending = ModelPackageGroupStatus' "Pending"

{-# COMPLETE
  ModelPackageGroupStatus_Completed,
  ModelPackageGroupStatus_DeleteFailed,
  ModelPackageGroupStatus_Deleting,
  ModelPackageGroupStatus_Failed,
  ModelPackageGroupStatus_InProgress,
  ModelPackageGroupStatus_Pending,
  ModelPackageGroupStatus'
  #-}
