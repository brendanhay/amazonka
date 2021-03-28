{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ConflictResolutionStrategyTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.ConflictResolutionStrategyTypeEnum
  ( ConflictResolutionStrategyTypeEnum
    ( ConflictResolutionStrategyTypeEnum'
    , ConflictResolutionStrategyTypeEnumNone
    , ConflictResolutionStrategyTypeEnumAcceptSource
    , ConflictResolutionStrategyTypeEnumAcceptDestination
    , ConflictResolutionStrategyTypeEnumAutomerge
    , fromConflictResolutionStrategyTypeEnum
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ConflictResolutionStrategyTypeEnum = ConflictResolutionStrategyTypeEnum'{fromConflictResolutionStrategyTypeEnum
                                                                                 :: Core.Text}
                                               deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                               Core.Show, Core.Generic)
                                               deriving newtype (Core.IsString, Core.Hashable,
                                                                 Core.NFData, Core.ToJSONKey,
                                                                 Core.FromJSONKey, Core.ToJSON,
                                                                 Core.FromJSON, Core.ToXML,
                                                                 Core.FromXML, Core.ToText,
                                                                 Core.FromText, Core.ToByteString,
                                                                 Core.ToQuery, Core.ToHeader)

pattern ConflictResolutionStrategyTypeEnumNone :: ConflictResolutionStrategyTypeEnum
pattern ConflictResolutionStrategyTypeEnumNone = ConflictResolutionStrategyTypeEnum' "NONE"

pattern ConflictResolutionStrategyTypeEnumAcceptSource :: ConflictResolutionStrategyTypeEnum
pattern ConflictResolutionStrategyTypeEnumAcceptSource = ConflictResolutionStrategyTypeEnum' "ACCEPT_SOURCE"

pattern ConflictResolutionStrategyTypeEnumAcceptDestination :: ConflictResolutionStrategyTypeEnum
pattern ConflictResolutionStrategyTypeEnumAcceptDestination = ConflictResolutionStrategyTypeEnum' "ACCEPT_DESTINATION"

pattern ConflictResolutionStrategyTypeEnumAutomerge :: ConflictResolutionStrategyTypeEnum
pattern ConflictResolutionStrategyTypeEnumAutomerge = ConflictResolutionStrategyTypeEnum' "AUTOMERGE"

{-# COMPLETE 
  ConflictResolutionStrategyTypeEnumNone,

  ConflictResolutionStrategyTypeEnumAcceptSource,

  ConflictResolutionStrategyTypeEnumAcceptDestination,

  ConflictResolutionStrategyTypeEnumAutomerge,
  ConflictResolutionStrategyTypeEnum'
  #-}
