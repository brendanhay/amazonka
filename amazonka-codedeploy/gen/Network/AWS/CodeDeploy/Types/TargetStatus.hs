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
-- Module      : Network.AWS.CodeDeploy.Types.TargetStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TargetStatus
  ( TargetStatus
      ( ..,
        TargetStatus_Failed,
        TargetStatus_InProgress,
        TargetStatus_Pending,
        TargetStatus_Ready,
        TargetStatus_Skipped,
        TargetStatus_Succeeded,
        TargetStatus_Unknown
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype TargetStatus = TargetStatus'
  { fromTargetStatus ::
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

pattern TargetStatus_Failed :: TargetStatus
pattern TargetStatus_Failed = TargetStatus' "Failed"

pattern TargetStatus_InProgress :: TargetStatus
pattern TargetStatus_InProgress = TargetStatus' "InProgress"

pattern TargetStatus_Pending :: TargetStatus
pattern TargetStatus_Pending = TargetStatus' "Pending"

pattern TargetStatus_Ready :: TargetStatus
pattern TargetStatus_Ready = TargetStatus' "Ready"

pattern TargetStatus_Skipped :: TargetStatus
pattern TargetStatus_Skipped = TargetStatus' "Skipped"

pattern TargetStatus_Succeeded :: TargetStatus
pattern TargetStatus_Succeeded = TargetStatus' "Succeeded"

pattern TargetStatus_Unknown :: TargetStatus
pattern TargetStatus_Unknown = TargetStatus' "Unknown"

{-# COMPLETE
  TargetStatus_Failed,
  TargetStatus_InProgress,
  TargetStatus_Pending,
  TargetStatus_Ready,
  TargetStatus_Skipped,
  TargetStatus_Succeeded,
  TargetStatus_Unknown,
  TargetStatus'
  #-}
