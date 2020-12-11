-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformSortColumnType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformSortColumnType
  ( TransformSortColumnType
      ( TransformSortColumnType',
        Created,
        LastModified,
        Name,
        Status,
        TransformType
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TransformSortColumnType = TransformSortColumnType' Lude.Text
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

pattern Created :: TransformSortColumnType
pattern Created = TransformSortColumnType' "CREATED"

pattern LastModified :: TransformSortColumnType
pattern LastModified = TransformSortColumnType' "LAST_MODIFIED"

pattern Name :: TransformSortColumnType
pattern Name = TransformSortColumnType' "NAME"

pattern Status :: TransformSortColumnType
pattern Status = TransformSortColumnType' "STATUS"

pattern TransformType :: TransformSortColumnType
pattern TransformType = TransformSortColumnType' "TRANSFORM_TYPE"

{-# COMPLETE
  Created,
  LastModified,
  Name,
  Status,
  TransformType,
  TransformSortColumnType'
  #-}
