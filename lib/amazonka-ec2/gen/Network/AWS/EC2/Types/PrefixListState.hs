{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrefixListState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PrefixListState
  ( PrefixListState
    ( PrefixListState'
    , PrefixListStateCreateInProgress
    , PrefixListStateCreateComplete
    , PrefixListStateCreateFailed
    , PrefixListStateModifyInProgress
    , PrefixListStateModifyComplete
    , PrefixListStateModifyFailed
    , PrefixListStateRestoreInProgress
    , PrefixListStateRestoreComplete
    , PrefixListStateRestoreFailed
    , PrefixListStateDeleteInProgress
    , PrefixListStateDeleteComplete
    , PrefixListStateDeleteFailed
    , fromPrefixListState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype PrefixListState = PrefixListState'{fromPrefixListState ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern PrefixListStateCreateInProgress :: PrefixListState
pattern PrefixListStateCreateInProgress = PrefixListState' "create-in-progress"

pattern PrefixListStateCreateComplete :: PrefixListState
pattern PrefixListStateCreateComplete = PrefixListState' "create-complete"

pattern PrefixListStateCreateFailed :: PrefixListState
pattern PrefixListStateCreateFailed = PrefixListState' "create-failed"

pattern PrefixListStateModifyInProgress :: PrefixListState
pattern PrefixListStateModifyInProgress = PrefixListState' "modify-in-progress"

pattern PrefixListStateModifyComplete :: PrefixListState
pattern PrefixListStateModifyComplete = PrefixListState' "modify-complete"

pattern PrefixListStateModifyFailed :: PrefixListState
pattern PrefixListStateModifyFailed = PrefixListState' "modify-failed"

pattern PrefixListStateRestoreInProgress :: PrefixListState
pattern PrefixListStateRestoreInProgress = PrefixListState' "restore-in-progress"

pattern PrefixListStateRestoreComplete :: PrefixListState
pattern PrefixListStateRestoreComplete = PrefixListState' "restore-complete"

pattern PrefixListStateRestoreFailed :: PrefixListState
pattern PrefixListStateRestoreFailed = PrefixListState' "restore-failed"

pattern PrefixListStateDeleteInProgress :: PrefixListState
pattern PrefixListStateDeleteInProgress = PrefixListState' "delete-in-progress"

pattern PrefixListStateDeleteComplete :: PrefixListState
pattern PrefixListStateDeleteComplete = PrefixListState' "delete-complete"

pattern PrefixListStateDeleteFailed :: PrefixListState
pattern PrefixListStateDeleteFailed = PrefixListState' "delete-failed"

{-# COMPLETE 
  PrefixListStateCreateInProgress,

  PrefixListStateCreateComplete,

  PrefixListStateCreateFailed,

  PrefixListStateModifyInProgress,

  PrefixListStateModifyComplete,

  PrefixListStateModifyFailed,

  PrefixListStateRestoreInProgress,

  PrefixListStateRestoreComplete,

  PrefixListStateRestoreFailed,

  PrefixListStateDeleteInProgress,

  PrefixListStateDeleteComplete,

  PrefixListStateDeleteFailed,
  PrefixListState'
  #-}
