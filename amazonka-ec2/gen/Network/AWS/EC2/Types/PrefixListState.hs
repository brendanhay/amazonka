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
-- Module      : Network.AWS.EC2.Types.PrefixListState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrefixListState
  ( PrefixListState
      ( ..,
        PrefixListState_Create_complete,
        PrefixListState_Create_failed,
        PrefixListState_Create_in_progress,
        PrefixListState_Delete_complete,
        PrefixListState_Delete_failed,
        PrefixListState_Delete_in_progress,
        PrefixListState_Modify_complete,
        PrefixListState_Modify_failed,
        PrefixListState_Modify_in_progress,
        PrefixListState_Restore_complete,
        PrefixListState_Restore_failed,
        PrefixListState_Restore_in_progress
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype PrefixListState = PrefixListState'
  { fromPrefixListState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern PrefixListState_Create_complete :: PrefixListState
pattern PrefixListState_Create_complete = PrefixListState' "create-complete"

pattern PrefixListState_Create_failed :: PrefixListState
pattern PrefixListState_Create_failed = PrefixListState' "create-failed"

pattern PrefixListState_Create_in_progress :: PrefixListState
pattern PrefixListState_Create_in_progress = PrefixListState' "create-in-progress"

pattern PrefixListState_Delete_complete :: PrefixListState
pattern PrefixListState_Delete_complete = PrefixListState' "delete-complete"

pattern PrefixListState_Delete_failed :: PrefixListState
pattern PrefixListState_Delete_failed = PrefixListState' "delete-failed"

pattern PrefixListState_Delete_in_progress :: PrefixListState
pattern PrefixListState_Delete_in_progress = PrefixListState' "delete-in-progress"

pattern PrefixListState_Modify_complete :: PrefixListState
pattern PrefixListState_Modify_complete = PrefixListState' "modify-complete"

pattern PrefixListState_Modify_failed :: PrefixListState
pattern PrefixListState_Modify_failed = PrefixListState' "modify-failed"

pattern PrefixListState_Modify_in_progress :: PrefixListState
pattern PrefixListState_Modify_in_progress = PrefixListState' "modify-in-progress"

pattern PrefixListState_Restore_complete :: PrefixListState
pattern PrefixListState_Restore_complete = PrefixListState' "restore-complete"

pattern PrefixListState_Restore_failed :: PrefixListState
pattern PrefixListState_Restore_failed = PrefixListState' "restore-failed"

pattern PrefixListState_Restore_in_progress :: PrefixListState
pattern PrefixListState_Restore_in_progress = PrefixListState' "restore-in-progress"

{-# COMPLETE
  PrefixListState_Create_complete,
  PrefixListState_Create_failed,
  PrefixListState_Create_in_progress,
  PrefixListState_Delete_complete,
  PrefixListState_Delete_failed,
  PrefixListState_Delete_in_progress,
  PrefixListState_Modify_complete,
  PrefixListState_Modify_failed,
  PrefixListState_Modify_in_progress,
  PrefixListState_Restore_complete,
  PrefixListState_Restore_failed,
  PrefixListState_Restore_in_progress,
  PrefixListState'
  #-}
