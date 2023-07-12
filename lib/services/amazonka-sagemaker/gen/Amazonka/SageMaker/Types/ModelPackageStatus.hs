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
-- Module      : Amazonka.SageMaker.Types.ModelPackageStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelPackageStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ModelPackageStatus = ModelPackageStatus'
  { fromModelPackageStatus ::
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
