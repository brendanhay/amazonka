{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ConflictDetailLevelTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.ConflictDetailLevelTypeEnum
  ( ConflictDetailLevelTypeEnum
    ( ConflictDetailLevelTypeEnum'
    , ConflictDetailLevelTypeEnumFileLevel
    , ConflictDetailLevelTypeEnumLineLevel
    , fromConflictDetailLevelTypeEnum
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ConflictDetailLevelTypeEnum = ConflictDetailLevelTypeEnum'{fromConflictDetailLevelTypeEnum
                                                                   :: Core.Text}
                                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                        Core.Generic)
                                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                          Core.ToJSONKey, Core.FromJSONKey,
                                                          Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                          Core.FromXML, Core.ToText, Core.FromText,
                                                          Core.ToByteString, Core.ToQuery,
                                                          Core.ToHeader)

pattern ConflictDetailLevelTypeEnumFileLevel :: ConflictDetailLevelTypeEnum
pattern ConflictDetailLevelTypeEnumFileLevel = ConflictDetailLevelTypeEnum' "FILE_LEVEL"

pattern ConflictDetailLevelTypeEnumLineLevel :: ConflictDetailLevelTypeEnum
pattern ConflictDetailLevelTypeEnumLineLevel = ConflictDetailLevelTypeEnum' "LINE_LEVEL"

{-# COMPLETE 
  ConflictDetailLevelTypeEnumFileLevel,

  ConflictDetailLevelTypeEnumLineLevel,
  ConflictDetailLevelTypeEnum'
  #-}
