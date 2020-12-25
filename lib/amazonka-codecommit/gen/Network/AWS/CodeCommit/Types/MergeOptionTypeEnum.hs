{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.MergeOptionTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.MergeOptionTypeEnum
  ( MergeOptionTypeEnum
      ( MergeOptionTypeEnum',
        MergeOptionTypeEnumFastForwardMerge,
        MergeOptionTypeEnumSquashMerge,
        MergeOptionTypeEnumThreeWayMerge,
        fromMergeOptionTypeEnum
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype MergeOptionTypeEnum = MergeOptionTypeEnum'
  { fromMergeOptionTypeEnum ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern MergeOptionTypeEnumFastForwardMerge :: MergeOptionTypeEnum
pattern MergeOptionTypeEnumFastForwardMerge = MergeOptionTypeEnum' "FAST_FORWARD_MERGE"

pattern MergeOptionTypeEnumSquashMerge :: MergeOptionTypeEnum
pattern MergeOptionTypeEnumSquashMerge = MergeOptionTypeEnum' "SQUASH_MERGE"

pattern MergeOptionTypeEnumThreeWayMerge :: MergeOptionTypeEnum
pattern MergeOptionTypeEnumThreeWayMerge = MergeOptionTypeEnum' "THREE_WAY_MERGE"

{-# COMPLETE
  MergeOptionTypeEnumFastForwardMerge,
  MergeOptionTypeEnumSquashMerge,
  MergeOptionTypeEnumThreeWayMerge,
  MergeOptionTypeEnum'
  #-}
