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
-- Module      : Network.AWS.EC2.Types.ReplaceRootVolumeTaskState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReplaceRootVolumeTaskState
  ( ReplaceRootVolumeTaskState
      ( ..,
        ReplaceRootVolumeTaskState_Failed,
        ReplaceRootVolumeTaskState_Failed_detached,
        ReplaceRootVolumeTaskState_Failing,
        ReplaceRootVolumeTaskState_In_progress,
        ReplaceRootVolumeTaskState_Pending,
        ReplaceRootVolumeTaskState_Succeeded
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype ReplaceRootVolumeTaskState = ReplaceRootVolumeTaskState'
  { fromReplaceRootVolumeTaskState ::
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

pattern ReplaceRootVolumeTaskState_Failed :: ReplaceRootVolumeTaskState
pattern ReplaceRootVolumeTaskState_Failed = ReplaceRootVolumeTaskState' "failed"

pattern ReplaceRootVolumeTaskState_Failed_detached :: ReplaceRootVolumeTaskState
pattern ReplaceRootVolumeTaskState_Failed_detached = ReplaceRootVolumeTaskState' "failed-detached"

pattern ReplaceRootVolumeTaskState_Failing :: ReplaceRootVolumeTaskState
pattern ReplaceRootVolumeTaskState_Failing = ReplaceRootVolumeTaskState' "failing"

pattern ReplaceRootVolumeTaskState_In_progress :: ReplaceRootVolumeTaskState
pattern ReplaceRootVolumeTaskState_In_progress = ReplaceRootVolumeTaskState' "in-progress"

pattern ReplaceRootVolumeTaskState_Pending :: ReplaceRootVolumeTaskState
pattern ReplaceRootVolumeTaskState_Pending = ReplaceRootVolumeTaskState' "pending"

pattern ReplaceRootVolumeTaskState_Succeeded :: ReplaceRootVolumeTaskState
pattern ReplaceRootVolumeTaskState_Succeeded = ReplaceRootVolumeTaskState' "succeeded"

{-# COMPLETE
  ReplaceRootVolumeTaskState_Failed,
  ReplaceRootVolumeTaskState_Failed_detached,
  ReplaceRootVolumeTaskState_Failing,
  ReplaceRootVolumeTaskState_In_progress,
  ReplaceRootVolumeTaskState_Pending,
  ReplaceRootVolumeTaskState_Succeeded,
  ReplaceRootVolumeTaskState'
  #-}
