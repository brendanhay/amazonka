{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformSortColumnType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformSortColumnType
  ( TransformSortColumnType
      ( ..,
        TransformSortColumnType_CREATED,
        TransformSortColumnType_LAST_MODIFIED,
        TransformSortColumnType_NAME,
        TransformSortColumnType_STATUS,
        TransformSortColumnType_TRANSFORM_TYPE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TransformSortColumnType = TransformSortColumnType'
  { fromTransformSortColumnType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern TransformSortColumnType_CREATED :: TransformSortColumnType
pattern TransformSortColumnType_CREATED = TransformSortColumnType' "CREATED"

pattern TransformSortColumnType_LAST_MODIFIED :: TransformSortColumnType
pattern TransformSortColumnType_LAST_MODIFIED = TransformSortColumnType' "LAST_MODIFIED"

pattern TransformSortColumnType_NAME :: TransformSortColumnType
pattern TransformSortColumnType_NAME = TransformSortColumnType' "NAME"

pattern TransformSortColumnType_STATUS :: TransformSortColumnType
pattern TransformSortColumnType_STATUS = TransformSortColumnType' "STATUS"

pattern TransformSortColumnType_TRANSFORM_TYPE :: TransformSortColumnType
pattern TransformSortColumnType_TRANSFORM_TYPE = TransformSortColumnType' "TRANSFORM_TYPE"

{-# COMPLETE
  TransformSortColumnType_CREATED,
  TransformSortColumnType_LAST_MODIFIED,
  TransformSortColumnType_NAME,
  TransformSortColumnType_STATUS,
  TransformSortColumnType_TRANSFORM_TYPE,
  TransformSortColumnType'
  #-}
