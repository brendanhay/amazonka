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
        FastForwardMerge,
        SquashMerge,
        ThreeWayMerge
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MergeOptionTypeEnum = MergeOptionTypeEnum' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern FastForwardMerge :: MergeOptionTypeEnum
pattern FastForwardMerge = MergeOptionTypeEnum' "FAST_FORWARD_MERGE"

pattern SquashMerge :: MergeOptionTypeEnum
pattern SquashMerge = MergeOptionTypeEnum' "SQUASH_MERGE"

pattern ThreeWayMerge :: MergeOptionTypeEnum
pattern ThreeWayMerge = MergeOptionTypeEnum' "THREE_WAY_MERGE"

{-# COMPLETE
  FastForwardMerge,
  SquashMerge,
  ThreeWayMerge,
  MergeOptionTypeEnum'
  #-}
