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
-- Module      : Network.AWS.QuickSight.Types.DataSourceErrorInfoType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.DataSourceErrorInfoType
  ( DataSourceErrorInfoType
      ( ..,
        DataSourceErrorInfoType_ACCESS_DENIED,
        DataSourceErrorInfoType_CONFLICT,
        DataSourceErrorInfoType_COPY_SOURCE_NOT_FOUND,
        DataSourceErrorInfoType_ENGINE_VERSION_NOT_SUPPORTED,
        DataSourceErrorInfoType_GENERIC_SQL_FAILURE,
        DataSourceErrorInfoType_TIMEOUT,
        DataSourceErrorInfoType_UNKNOWN,
        DataSourceErrorInfoType_UNKNOWN_HOST
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DataSourceErrorInfoType = DataSourceErrorInfoType'
  { fromDataSourceErrorInfoType ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern DataSourceErrorInfoType_ACCESS_DENIED :: DataSourceErrorInfoType
pattern DataSourceErrorInfoType_ACCESS_DENIED = DataSourceErrorInfoType' "ACCESS_DENIED"

pattern DataSourceErrorInfoType_CONFLICT :: DataSourceErrorInfoType
pattern DataSourceErrorInfoType_CONFLICT = DataSourceErrorInfoType' "CONFLICT"

pattern DataSourceErrorInfoType_COPY_SOURCE_NOT_FOUND :: DataSourceErrorInfoType
pattern DataSourceErrorInfoType_COPY_SOURCE_NOT_FOUND = DataSourceErrorInfoType' "COPY_SOURCE_NOT_FOUND"

pattern DataSourceErrorInfoType_ENGINE_VERSION_NOT_SUPPORTED :: DataSourceErrorInfoType
pattern DataSourceErrorInfoType_ENGINE_VERSION_NOT_SUPPORTED = DataSourceErrorInfoType' "ENGINE_VERSION_NOT_SUPPORTED"

pattern DataSourceErrorInfoType_GENERIC_SQL_FAILURE :: DataSourceErrorInfoType
pattern DataSourceErrorInfoType_GENERIC_SQL_FAILURE = DataSourceErrorInfoType' "GENERIC_SQL_FAILURE"

pattern DataSourceErrorInfoType_TIMEOUT :: DataSourceErrorInfoType
pattern DataSourceErrorInfoType_TIMEOUT = DataSourceErrorInfoType' "TIMEOUT"

pattern DataSourceErrorInfoType_UNKNOWN :: DataSourceErrorInfoType
pattern DataSourceErrorInfoType_UNKNOWN = DataSourceErrorInfoType' "UNKNOWN"

pattern DataSourceErrorInfoType_UNKNOWN_HOST :: DataSourceErrorInfoType
pattern DataSourceErrorInfoType_UNKNOWN_HOST = DataSourceErrorInfoType' "UNKNOWN_HOST"

{-# COMPLETE
  DataSourceErrorInfoType_ACCESS_DENIED,
  DataSourceErrorInfoType_CONFLICT,
  DataSourceErrorInfoType_COPY_SOURCE_NOT_FOUND,
  DataSourceErrorInfoType_ENGINE_VERSION_NOT_SUPPORTED,
  DataSourceErrorInfoType_GENERIC_SQL_FAILURE,
  DataSourceErrorInfoType_TIMEOUT,
  DataSourceErrorInfoType_UNKNOWN,
  DataSourceErrorInfoType_UNKNOWN_HOST,
  DataSourceErrorInfoType'
  #-}
