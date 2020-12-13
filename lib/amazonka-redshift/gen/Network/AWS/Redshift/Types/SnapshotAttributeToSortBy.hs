{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SnapshotAttributeToSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SnapshotAttributeToSortBy
  ( SnapshotAttributeToSortBy
      ( SnapshotAttributeToSortBy',
        SourceType,
        TotalSize,
        CreateTime
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

newtype SnapshotAttributeToSortBy = SnapshotAttributeToSortBy' Lude.Text
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

pattern SourceType :: SnapshotAttributeToSortBy
pattern SourceType = SnapshotAttributeToSortBy' "SOURCE_TYPE"

pattern TotalSize :: SnapshotAttributeToSortBy
pattern TotalSize = SnapshotAttributeToSortBy' "TOTAL_SIZE"

pattern CreateTime :: SnapshotAttributeToSortBy
pattern CreateTime = SnapshotAttributeToSortBy' "CREATE_TIME"

{-# COMPLETE
  SourceType,
  TotalSize,
  CreateTime,
  SnapshotAttributeToSortBy'
  #-}
