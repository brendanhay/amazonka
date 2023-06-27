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
-- Module      : Amazonka.QuickSight.Types.AssetBundleExportJobVPCConnectionPropertyToOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleExportJobVPCConnectionPropertyToOverride
  ( AssetBundleExportJobVPCConnectionPropertyToOverride
      ( ..,
        AssetBundleExportJobVPCConnectionPropertyToOverride_DnsResolvers,
        AssetBundleExportJobVPCConnectionPropertyToOverride_Name,
        AssetBundleExportJobVPCConnectionPropertyToOverride_RoleArn
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AssetBundleExportJobVPCConnectionPropertyToOverride = AssetBundleExportJobVPCConnectionPropertyToOverride'
  { fromAssetBundleExportJobVPCConnectionPropertyToOverride ::
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

pattern AssetBundleExportJobVPCConnectionPropertyToOverride_DnsResolvers :: AssetBundleExportJobVPCConnectionPropertyToOverride
pattern AssetBundleExportJobVPCConnectionPropertyToOverride_DnsResolvers = AssetBundleExportJobVPCConnectionPropertyToOverride' "DnsResolvers"

pattern AssetBundleExportJobVPCConnectionPropertyToOverride_Name :: AssetBundleExportJobVPCConnectionPropertyToOverride
pattern AssetBundleExportJobVPCConnectionPropertyToOverride_Name = AssetBundleExportJobVPCConnectionPropertyToOverride' "Name"

pattern AssetBundleExportJobVPCConnectionPropertyToOverride_RoleArn :: AssetBundleExportJobVPCConnectionPropertyToOverride
pattern AssetBundleExportJobVPCConnectionPropertyToOverride_RoleArn = AssetBundleExportJobVPCConnectionPropertyToOverride' "RoleArn"

{-# COMPLETE
  AssetBundleExportJobVPCConnectionPropertyToOverride_DnsResolvers,
  AssetBundleExportJobVPCConnectionPropertyToOverride_Name,
  AssetBundleExportJobVPCConnectionPropertyToOverride_RoleArn,
  AssetBundleExportJobVPCConnectionPropertyToOverride'
  #-}
