{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Glue.Types.TransformSortColumnType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TransformSortColumnType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TransformSortColumnType = TransformSortColumnType'
  { fromTransformSortColumnType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
