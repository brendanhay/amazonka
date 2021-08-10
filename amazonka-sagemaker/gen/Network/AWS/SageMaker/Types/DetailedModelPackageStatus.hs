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
-- Module      : Network.AWS.SageMaker.Types.DetailedModelPackageStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DetailedModelPackageStatus
  ( DetailedModelPackageStatus
      ( ..,
        DetailedModelPackageStatus_Completed,
        DetailedModelPackageStatus_Failed,
        DetailedModelPackageStatus_InProgress,
        DetailedModelPackageStatus_NotStarted
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DetailedModelPackageStatus = DetailedModelPackageStatus'
  { fromDetailedModelPackageStatus ::
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

pattern DetailedModelPackageStatus_Completed :: DetailedModelPackageStatus
pattern DetailedModelPackageStatus_Completed = DetailedModelPackageStatus' "Completed"

pattern DetailedModelPackageStatus_Failed :: DetailedModelPackageStatus
pattern DetailedModelPackageStatus_Failed = DetailedModelPackageStatus' "Failed"

pattern DetailedModelPackageStatus_InProgress :: DetailedModelPackageStatus
pattern DetailedModelPackageStatus_InProgress = DetailedModelPackageStatus' "InProgress"

pattern DetailedModelPackageStatus_NotStarted :: DetailedModelPackageStatus
pattern DetailedModelPackageStatus_NotStarted = DetailedModelPackageStatus' "NotStarted"

{-# COMPLETE
  DetailedModelPackageStatus_Completed,
  DetailedModelPackageStatus_Failed,
  DetailedModelPackageStatus_InProgress,
  DetailedModelPackageStatus_NotStarted,
  DetailedModelPackageStatus'
  #-}
