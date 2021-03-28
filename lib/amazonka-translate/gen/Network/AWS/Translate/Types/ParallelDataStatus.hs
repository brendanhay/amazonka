{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.ParallelDataStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Translate.Types.ParallelDataStatus
  ( ParallelDataStatus
    ( ParallelDataStatus'
    , ParallelDataStatusCreating
    , ParallelDataStatusUpdating
    , ParallelDataStatusActive
    , ParallelDataStatusDeleting
    , ParallelDataStatusFailed
    , fromParallelDataStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ParallelDataStatus = ParallelDataStatus'{fromParallelDataStatus
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern ParallelDataStatusCreating :: ParallelDataStatus
pattern ParallelDataStatusCreating = ParallelDataStatus' "CREATING"

pattern ParallelDataStatusUpdating :: ParallelDataStatus
pattern ParallelDataStatusUpdating = ParallelDataStatus' "UPDATING"

pattern ParallelDataStatusActive :: ParallelDataStatus
pattern ParallelDataStatusActive = ParallelDataStatus' "ACTIVE"

pattern ParallelDataStatusDeleting :: ParallelDataStatus
pattern ParallelDataStatusDeleting = ParallelDataStatus' "DELETING"

pattern ParallelDataStatusFailed :: ParallelDataStatus
pattern ParallelDataStatusFailed = ParallelDataStatus' "FAILED"

{-# COMPLETE 
  ParallelDataStatusCreating,

  ParallelDataStatusUpdating,

  ParallelDataStatusActive,

  ParallelDataStatusDeleting,

  ParallelDataStatusFailed,
  ParallelDataStatus'
  #-}
