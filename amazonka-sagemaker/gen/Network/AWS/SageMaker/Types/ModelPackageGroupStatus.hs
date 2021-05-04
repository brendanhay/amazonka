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
-- Module      : Network.AWS.SageMaker.Types.ModelPackageGroupStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageGroupStatus
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

import qualified Network.AWS.Prelude as Prelude

newtype ModelPackageGroupStatus = ModelPackageGroupStatus'
  { fromModelPackageGroupStatus ::
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
