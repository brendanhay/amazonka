-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.MergeStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.MergeStrategy
  ( MergeStrategy
      ( MergeStrategy',
        FailOnConflict,
        OverwriteLatest
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MergeStrategy = MergeStrategy' Lude.Text
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

pattern FailOnConflict :: MergeStrategy
pattern FailOnConflict = MergeStrategy' "FAIL_ON_CONFLICT"

pattern OverwriteLatest :: MergeStrategy
pattern OverwriteLatest = MergeStrategy' "OVERWRITE_LATEST"

{-# COMPLETE
  FailOnConflict,
  OverwriteLatest,
  MergeStrategy'
  #-}
