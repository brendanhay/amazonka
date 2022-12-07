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
-- Module      : Amazonka.CodeDeploy.Types.LifecycleEventStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.LifecycleEventStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LifecycleEventStatus = LifecycleEventStatus'
  { fromLifecycleEventStatus ::
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
