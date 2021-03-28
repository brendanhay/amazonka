{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformSortColumnType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.TransformSortColumnType
  ( TransformSortColumnType
    ( TransformSortColumnType'
    , TransformSortColumnTypeName
    , TransformSortColumnTypeTransformType
    , TransformSortColumnTypeStatus
    , TransformSortColumnTypeCreated
    , TransformSortColumnTypeLastModified
    , fromTransformSortColumnType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype TransformSortColumnType = TransformSortColumnType'{fromTransformSortColumnType
                                                           :: Core.Text}
                                    deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                    Core.Generic)
                                    deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                      Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                      Core.FromJSON, Core.ToXML, Core.FromXML,
                                                      Core.ToText, Core.FromText, Core.ToByteString,
                                                      Core.ToQuery, Core.ToHeader)

pattern TransformSortColumnTypeName :: TransformSortColumnType
pattern TransformSortColumnTypeName = TransformSortColumnType' "NAME"

pattern TransformSortColumnTypeTransformType :: TransformSortColumnType
pattern TransformSortColumnTypeTransformType = TransformSortColumnType' "TRANSFORM_TYPE"

pattern TransformSortColumnTypeStatus :: TransformSortColumnType
pattern TransformSortColumnTypeStatus = TransformSortColumnType' "STATUS"

pattern TransformSortColumnTypeCreated :: TransformSortColumnType
pattern TransformSortColumnTypeCreated = TransformSortColumnType' "CREATED"

pattern TransformSortColumnTypeLastModified :: TransformSortColumnType
pattern TransformSortColumnTypeLastModified = TransformSortColumnType' "LAST_MODIFIED"

{-# COMPLETE 
  TransformSortColumnTypeName,

  TransformSortColumnTypeTransformType,

  TransformSortColumnTypeStatus,

  TransformSortColumnTypeCreated,

  TransformSortColumnTypeLastModified,
  TransformSortColumnType'
  #-}
