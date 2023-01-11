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
-- Module      : Amazonka.DataExchange.Types.Type
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.Type
  ( Type
      ( ..,
        Type_CREATE_S3_DATA_ACCESS_FROM_S3_BUCKET,
        Type_EXPORT_ASSETS_TO_S3,
        Type_EXPORT_ASSET_TO_SIGNED_URL,
        Type_EXPORT_REVISIONS_TO_S3,
        Type_IMPORT_ASSETS_FROM_LAKE_FORMATION_TAG_POLICY,
        Type_IMPORT_ASSETS_FROM_REDSHIFT_DATA_SHARES,
        Type_IMPORT_ASSETS_FROM_S3,
        Type_IMPORT_ASSET_FROM_API_GATEWAY_API,
        Type_IMPORT_ASSET_FROM_SIGNED_URL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Type = Type' {fromType :: Data.Text}
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

pattern Type_CREATE_S3_DATA_ACCESS_FROM_S3_BUCKET :: Type
pattern Type_CREATE_S3_DATA_ACCESS_FROM_S3_BUCKET = Type' "CREATE_S3_DATA_ACCESS_FROM_S3_BUCKET"

pattern Type_EXPORT_ASSETS_TO_S3 :: Type
pattern Type_EXPORT_ASSETS_TO_S3 = Type' "EXPORT_ASSETS_TO_S3"

pattern Type_EXPORT_ASSET_TO_SIGNED_URL :: Type
pattern Type_EXPORT_ASSET_TO_SIGNED_URL = Type' "EXPORT_ASSET_TO_SIGNED_URL"

pattern Type_EXPORT_REVISIONS_TO_S3 :: Type
pattern Type_EXPORT_REVISIONS_TO_S3 = Type' "EXPORT_REVISIONS_TO_S3"

pattern Type_IMPORT_ASSETS_FROM_LAKE_FORMATION_TAG_POLICY :: Type
pattern Type_IMPORT_ASSETS_FROM_LAKE_FORMATION_TAG_POLICY = Type' "IMPORT_ASSETS_FROM_LAKE_FORMATION_TAG_POLICY"

pattern Type_IMPORT_ASSETS_FROM_REDSHIFT_DATA_SHARES :: Type
pattern Type_IMPORT_ASSETS_FROM_REDSHIFT_DATA_SHARES = Type' "IMPORT_ASSETS_FROM_REDSHIFT_DATA_SHARES"

pattern Type_IMPORT_ASSETS_FROM_S3 :: Type
pattern Type_IMPORT_ASSETS_FROM_S3 = Type' "IMPORT_ASSETS_FROM_S3"

pattern Type_IMPORT_ASSET_FROM_API_GATEWAY_API :: Type
pattern Type_IMPORT_ASSET_FROM_API_GATEWAY_API = Type' "IMPORT_ASSET_FROM_API_GATEWAY_API"

pattern Type_IMPORT_ASSET_FROM_SIGNED_URL :: Type
pattern Type_IMPORT_ASSET_FROM_SIGNED_URL = Type' "IMPORT_ASSET_FROM_SIGNED_URL"

{-# COMPLETE
  Type_CREATE_S3_DATA_ACCESS_FROM_S3_BUCKET,
  Type_EXPORT_ASSETS_TO_S3,
  Type_EXPORT_ASSET_TO_SIGNED_URL,
  Type_EXPORT_REVISIONS_TO_S3,
  Type_IMPORT_ASSETS_FROM_LAKE_FORMATION_TAG_POLICY,
  Type_IMPORT_ASSETS_FROM_REDSHIFT_DATA_SHARES,
  Type_IMPORT_ASSETS_FROM_S3,
  Type_IMPORT_ASSET_FROM_API_GATEWAY_API,
  Type_IMPORT_ASSET_FROM_SIGNED_URL,
  Type'
  #-}
