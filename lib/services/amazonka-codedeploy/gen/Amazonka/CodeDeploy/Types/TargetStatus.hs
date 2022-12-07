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
-- Module      : Amazonka.CodeDeploy.Types.TargetStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.TargetStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TargetStatus = TargetStatus'
  { fromTargetStatus ::
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
