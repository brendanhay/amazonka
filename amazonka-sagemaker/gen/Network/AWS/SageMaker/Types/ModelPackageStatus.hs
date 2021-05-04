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
-- Module      : Network.AWS.SageMaker.Types.ModelPackageStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageStatus
  ( ModelPackageStatus
      ( ..,
        ModelPackageStatus_Completed,
        ModelPackageStatus_Deleting,
        ModelPackageStatus_Failed,
        ModelPackageStatus_InProgress,
        ModelPackageStatus_Pending
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ModelPackageStatus = ModelPackageStatus'
  { fromModelPackageStatus ::
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

pattern ModelPackageStatus_Completed :: ModelPackageStatus
pattern ModelPackageStatus_Completed = ModelPackageStatus' "Completed"

pattern ModelPackageStatus_Deleting :: ModelPackageStatus
pattern ModelPackageStatus_Deleting = ModelPackageStatus' "Deleting"

pattern ModelPackageStatus_Failed :: ModelPackageStatus
pattern ModelPackageStatus_Failed = ModelPackageStatus' "Failed"

pattern ModelPackageStatus_InProgress :: ModelPackageStatus
pattern ModelPackageStatus_InProgress = ModelPackageStatus' "InProgress"

pattern ModelPackageStatus_Pending :: ModelPackageStatus
pattern ModelPackageStatus_Pending = ModelPackageStatus' "Pending"

{-# COMPLETE
  ModelPackageStatus_Completed,
  ModelPackageStatus_Deleting,
  ModelPackageStatus_Failed,
  ModelPackageStatus_InProgress,
  ModelPackageStatus_Pending,
  ModelPackageStatus'
  #-}
