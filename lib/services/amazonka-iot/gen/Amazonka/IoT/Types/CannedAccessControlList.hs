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
-- Module      : Amazonka.IoT.Types.CannedAccessControlList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.CannedAccessControlList
  ( CannedAccessControlList
      ( ..,
        CannedAccessControlList_Authenticated_read,
        CannedAccessControlList_Aws_exec_read,
        CannedAccessControlList_Bucket_owner_full_control,
        CannedAccessControlList_Bucket_owner_read,
        CannedAccessControlList_Log_delivery_write,
        CannedAccessControlList_Private,
        CannedAccessControlList_Public_read,
        CannedAccessControlList_Public_read_write
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CannedAccessControlList = CannedAccessControlList'
  { fromCannedAccessControlList ::
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

pattern CannedAccessControlList_Authenticated_read :: CannedAccessControlList
pattern CannedAccessControlList_Authenticated_read = CannedAccessControlList' "authenticated-read"

pattern CannedAccessControlList_Aws_exec_read :: CannedAccessControlList
pattern CannedAccessControlList_Aws_exec_read = CannedAccessControlList' "aws-exec-read"

pattern CannedAccessControlList_Bucket_owner_full_control :: CannedAccessControlList
pattern CannedAccessControlList_Bucket_owner_full_control = CannedAccessControlList' "bucket-owner-full-control"

pattern CannedAccessControlList_Bucket_owner_read :: CannedAccessControlList
pattern CannedAccessControlList_Bucket_owner_read = CannedAccessControlList' "bucket-owner-read"

pattern CannedAccessControlList_Log_delivery_write :: CannedAccessControlList
pattern CannedAccessControlList_Log_delivery_write = CannedAccessControlList' "log-delivery-write"

pattern CannedAccessControlList_Private :: CannedAccessControlList
pattern CannedAccessControlList_Private = CannedAccessControlList' "private"

pattern CannedAccessControlList_Public_read :: CannedAccessControlList
pattern CannedAccessControlList_Public_read = CannedAccessControlList' "public-read"

pattern CannedAccessControlList_Public_read_write :: CannedAccessControlList
pattern CannedAccessControlList_Public_read_write = CannedAccessControlList' "public-read-write"

{-# COMPLETE
  CannedAccessControlList_Authenticated_read,
  CannedAccessControlList_Aws_exec_read,
  CannedAccessControlList_Bucket_owner_full_control,
  CannedAccessControlList_Bucket_owner_read,
  CannedAccessControlList_Log_delivery_write,
  CannedAccessControlList_Private,
  CannedAccessControlList_Public_read,
  CannedAccessControlList_Public_read_write,
  CannedAccessControlList'
  #-}
