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
-- Module      : Amazonka.DataExchange.Types.AssetType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.AssetType
  ( AssetType
      ( ..,
        AssetType_API_GATEWAY_API,
        AssetType_LAKE_FORMATION_DATA_PERMISSION,
        AssetType_REDSHIFT_DATA_SHARE,
        AssetType_S3_DATA_ACCESS,
        AssetType_S3_SNAPSHOT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AssetType = AssetType'
  { fromAssetType ::
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

pattern AssetType_API_GATEWAY_API :: AssetType
pattern AssetType_API_GATEWAY_API = AssetType' "API_GATEWAY_API"

pattern AssetType_LAKE_FORMATION_DATA_PERMISSION :: AssetType
pattern AssetType_LAKE_FORMATION_DATA_PERMISSION = AssetType' "LAKE_FORMATION_DATA_PERMISSION"

pattern AssetType_REDSHIFT_DATA_SHARE :: AssetType
pattern AssetType_REDSHIFT_DATA_SHARE = AssetType' "REDSHIFT_DATA_SHARE"

pattern AssetType_S3_DATA_ACCESS :: AssetType
pattern AssetType_S3_DATA_ACCESS = AssetType' "S3_DATA_ACCESS"

pattern AssetType_S3_SNAPSHOT :: AssetType
pattern AssetType_S3_SNAPSHOT = AssetType' "S3_SNAPSHOT"

{-# COMPLETE
  AssetType_API_GATEWAY_API,
  AssetType_LAKE_FORMATION_DATA_PERMISSION,
  AssetType_REDSHIFT_DATA_SHARE,
  AssetType_S3_DATA_ACCESS,
  AssetType_S3_SNAPSHOT,
  AssetType'
  #-}
