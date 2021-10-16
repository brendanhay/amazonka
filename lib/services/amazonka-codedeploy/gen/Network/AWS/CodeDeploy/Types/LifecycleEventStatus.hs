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
-- Module      : Network.AWS.CodeDeploy.Types.LifecycleEventStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LifecycleEventStatus
  ( LifecycleEventStatus
      ( ..,
        LifecycleEventStatus_Failed,
        LifecycleEventStatus_InProgress,
        LifecycleEventStatus_Pending,
        LifecycleEventStatus_Skipped,
        LifecycleEventStatus_Succeeded,
        LifecycleEventStatus_Unknown
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype LifecycleEventStatus = LifecycleEventStatus'
  { fromLifecycleEventStatus ::
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

pattern LifecycleEventStatus_Failed :: LifecycleEventStatus
pattern LifecycleEventStatus_Failed = LifecycleEventStatus' "Failed"

pattern LifecycleEventStatus_InProgress :: LifecycleEventStatus
pattern LifecycleEventStatus_InProgress = LifecycleEventStatus' "InProgress"

pattern LifecycleEventStatus_Pending :: LifecycleEventStatus
pattern LifecycleEventStatus_Pending = LifecycleEventStatus' "Pending"

pattern LifecycleEventStatus_Skipped :: LifecycleEventStatus
pattern LifecycleEventStatus_Skipped = LifecycleEventStatus' "Skipped"

pattern LifecycleEventStatus_Succeeded :: LifecycleEventStatus
pattern LifecycleEventStatus_Succeeded = LifecycleEventStatus' "Succeeded"

pattern LifecycleEventStatus_Unknown :: LifecycleEventStatus
pattern LifecycleEventStatus_Unknown = LifecycleEventStatus' "Unknown"

{-# COMPLETE
  LifecycleEventStatus_Failed,
  LifecycleEventStatus_InProgress,
  LifecycleEventStatus_Pending,
  LifecycleEventStatus_Skipped,
  LifecycleEventStatus_Succeeded,
  LifecycleEventStatus_Unknown,
  LifecycleEventStatus'
  #-}
