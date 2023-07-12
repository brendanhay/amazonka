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
-- Module      : Amazonka.DMS.Types.CannedAclForObjectsValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.CannedAclForObjectsValue
  ( CannedAclForObjectsValue
      ( ..,
        CannedAclForObjectsValue_Authenticated_read,
        CannedAclForObjectsValue_Aws_exec_read,
        CannedAclForObjectsValue_Bucket_owner_full_control,
        CannedAclForObjectsValue_Bucket_owner_read,
        CannedAclForObjectsValue_None,
        CannedAclForObjectsValue_Private,
        CannedAclForObjectsValue_Public_read,
        CannedAclForObjectsValue_Public_read_write
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CannedAclForObjectsValue = CannedAclForObjectsValue'
  { fromCannedAclForObjectsValue ::
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

pattern CannedAclForObjectsValue_Authenticated_read :: CannedAclForObjectsValue
pattern CannedAclForObjectsValue_Authenticated_read = CannedAclForObjectsValue' "authenticated-read"

pattern CannedAclForObjectsValue_Aws_exec_read :: CannedAclForObjectsValue
pattern CannedAclForObjectsValue_Aws_exec_read = CannedAclForObjectsValue' "aws-exec-read"

pattern CannedAclForObjectsValue_Bucket_owner_full_control :: CannedAclForObjectsValue
pattern CannedAclForObjectsValue_Bucket_owner_full_control = CannedAclForObjectsValue' "bucket-owner-full-control"

pattern CannedAclForObjectsValue_Bucket_owner_read :: CannedAclForObjectsValue
pattern CannedAclForObjectsValue_Bucket_owner_read = CannedAclForObjectsValue' "bucket-owner-read"

pattern CannedAclForObjectsValue_None :: CannedAclForObjectsValue
pattern CannedAclForObjectsValue_None = CannedAclForObjectsValue' "none"

pattern CannedAclForObjectsValue_Private :: CannedAclForObjectsValue
pattern CannedAclForObjectsValue_Private = CannedAclForObjectsValue' "private"

pattern CannedAclForObjectsValue_Public_read :: CannedAclForObjectsValue
pattern CannedAclForObjectsValue_Public_read = CannedAclForObjectsValue' "public-read"

pattern CannedAclForObjectsValue_Public_read_write :: CannedAclForObjectsValue
pattern CannedAclForObjectsValue_Public_read_write = CannedAclForObjectsValue' "public-read-write"

{-# COMPLETE
  CannedAclForObjectsValue_Authenticated_read,
  CannedAclForObjectsValue_Aws_exec_read,
  CannedAclForObjectsValue_Bucket_owner_full_control,
  CannedAclForObjectsValue_Bucket_owner_read,
  CannedAclForObjectsValue_None,
  CannedAclForObjectsValue_Private,
  CannedAclForObjectsValue_Public_read,
  CannedAclForObjectsValue_Public_read_write,
  CannedAclForObjectsValue'
  #-}
