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
-- Module      : Amazonka.MacieV2.Types.AllowListStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.AllowListStatusCode
  ( AllowListStatusCode
      ( ..,
        AllowListStatusCode_OK,
        AllowListStatusCode_S3_OBJECT_ACCESS_DENIED,
        AllowListStatusCode_S3_OBJECT_EMPTY,
        AllowListStatusCode_S3_OBJECT_NOT_FOUND,
        AllowListStatusCode_S3_OBJECT_OVERSIZE,
        AllowListStatusCode_S3_THROTTLED,
        AllowListStatusCode_S3_USER_ACCESS_DENIED,
        AllowListStatusCode_UNKNOWN_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates the current status of an allow list. Depending on the type of
-- criteria that the list specifies, possible values are:
newtype AllowListStatusCode = AllowListStatusCode'
  { fromAllowListStatusCode ::
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

pattern AllowListStatusCode_OK :: AllowListStatusCode
pattern AllowListStatusCode_OK = AllowListStatusCode' "OK"

pattern AllowListStatusCode_S3_OBJECT_ACCESS_DENIED :: AllowListStatusCode
pattern AllowListStatusCode_S3_OBJECT_ACCESS_DENIED = AllowListStatusCode' "S3_OBJECT_ACCESS_DENIED"

pattern AllowListStatusCode_S3_OBJECT_EMPTY :: AllowListStatusCode
pattern AllowListStatusCode_S3_OBJECT_EMPTY = AllowListStatusCode' "S3_OBJECT_EMPTY"

pattern AllowListStatusCode_S3_OBJECT_NOT_FOUND :: AllowListStatusCode
pattern AllowListStatusCode_S3_OBJECT_NOT_FOUND = AllowListStatusCode' "S3_OBJECT_NOT_FOUND"

pattern AllowListStatusCode_S3_OBJECT_OVERSIZE :: AllowListStatusCode
pattern AllowListStatusCode_S3_OBJECT_OVERSIZE = AllowListStatusCode' "S3_OBJECT_OVERSIZE"

pattern AllowListStatusCode_S3_THROTTLED :: AllowListStatusCode
pattern AllowListStatusCode_S3_THROTTLED = AllowListStatusCode' "S3_THROTTLED"

pattern AllowListStatusCode_S3_USER_ACCESS_DENIED :: AllowListStatusCode
pattern AllowListStatusCode_S3_USER_ACCESS_DENIED = AllowListStatusCode' "S3_USER_ACCESS_DENIED"

pattern AllowListStatusCode_UNKNOWN_ERROR :: AllowListStatusCode
pattern AllowListStatusCode_UNKNOWN_ERROR = AllowListStatusCode' "UNKNOWN_ERROR"

{-# COMPLETE
  AllowListStatusCode_OK,
  AllowListStatusCode_S3_OBJECT_ACCESS_DENIED,
  AllowListStatusCode_S3_OBJECT_EMPTY,
  AllowListStatusCode_S3_OBJECT_NOT_FOUND,
  AllowListStatusCode_S3_OBJECT_OVERSIZE,
  AllowListStatusCode_S3_THROTTLED,
  AllowListStatusCode_S3_USER_ACCESS_DENIED,
  AllowListStatusCode_UNKNOWN_ERROR,
  AllowListStatusCode'
  #-}
