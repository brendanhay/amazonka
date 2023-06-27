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
-- Module      : Amazonka.QuickSight.Types.AssetBundleExportJobDataSourcePropertyToOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleExportJobDataSourcePropertyToOverride
  ( AssetBundleExportJobDataSourcePropertyToOverride
      ( ..,
        AssetBundleExportJobDataSourcePropertyToOverride_Catalog,
        AssetBundleExportJobDataSourcePropertyToOverride_ClusterId,
        AssetBundleExportJobDataSourcePropertyToOverride_DataSetName,
        AssetBundleExportJobDataSourcePropertyToOverride_Database,
        AssetBundleExportJobDataSourcePropertyToOverride_DisableSsl,
        AssetBundleExportJobDataSourcePropertyToOverride_Domain,
        AssetBundleExportJobDataSourcePropertyToOverride_Host,
        AssetBundleExportJobDataSourcePropertyToOverride_InstanceId,
        AssetBundleExportJobDataSourcePropertyToOverride_ManifestFileLocation,
        AssetBundleExportJobDataSourcePropertyToOverride_Name,
        AssetBundleExportJobDataSourcePropertyToOverride_Password,
        AssetBundleExportJobDataSourcePropertyToOverride_Port,
        AssetBundleExportJobDataSourcePropertyToOverride_RoleArn,
        AssetBundleExportJobDataSourcePropertyToOverride_SecretArn,
        AssetBundleExportJobDataSourcePropertyToOverride_Username,
        AssetBundleExportJobDataSourcePropertyToOverride_Warehouse,
        AssetBundleExportJobDataSourcePropertyToOverride_WorkGroup
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AssetBundleExportJobDataSourcePropertyToOverride = AssetBundleExportJobDataSourcePropertyToOverride'
  { fromAssetBundleExportJobDataSourcePropertyToOverride ::
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

pattern AssetBundleExportJobDataSourcePropertyToOverride_Catalog :: AssetBundleExportJobDataSourcePropertyToOverride
pattern AssetBundleExportJobDataSourcePropertyToOverride_Catalog = AssetBundleExportJobDataSourcePropertyToOverride' "Catalog"

pattern AssetBundleExportJobDataSourcePropertyToOverride_ClusterId :: AssetBundleExportJobDataSourcePropertyToOverride
pattern AssetBundleExportJobDataSourcePropertyToOverride_ClusterId = AssetBundleExportJobDataSourcePropertyToOverride' "ClusterId"

pattern AssetBundleExportJobDataSourcePropertyToOverride_DataSetName :: AssetBundleExportJobDataSourcePropertyToOverride
pattern AssetBundleExportJobDataSourcePropertyToOverride_DataSetName = AssetBundleExportJobDataSourcePropertyToOverride' "DataSetName"

pattern AssetBundleExportJobDataSourcePropertyToOverride_Database :: AssetBundleExportJobDataSourcePropertyToOverride
pattern AssetBundleExportJobDataSourcePropertyToOverride_Database = AssetBundleExportJobDataSourcePropertyToOverride' "Database"

pattern AssetBundleExportJobDataSourcePropertyToOverride_DisableSsl :: AssetBundleExportJobDataSourcePropertyToOverride
pattern AssetBundleExportJobDataSourcePropertyToOverride_DisableSsl = AssetBundleExportJobDataSourcePropertyToOverride' "DisableSsl"

pattern AssetBundleExportJobDataSourcePropertyToOverride_Domain :: AssetBundleExportJobDataSourcePropertyToOverride
pattern AssetBundleExportJobDataSourcePropertyToOverride_Domain = AssetBundleExportJobDataSourcePropertyToOverride' "Domain"

pattern AssetBundleExportJobDataSourcePropertyToOverride_Host :: AssetBundleExportJobDataSourcePropertyToOverride
pattern AssetBundleExportJobDataSourcePropertyToOverride_Host = AssetBundleExportJobDataSourcePropertyToOverride' "Host"

pattern AssetBundleExportJobDataSourcePropertyToOverride_InstanceId :: AssetBundleExportJobDataSourcePropertyToOverride
pattern AssetBundleExportJobDataSourcePropertyToOverride_InstanceId = AssetBundleExportJobDataSourcePropertyToOverride' "InstanceId"

pattern AssetBundleExportJobDataSourcePropertyToOverride_ManifestFileLocation :: AssetBundleExportJobDataSourcePropertyToOverride
pattern AssetBundleExportJobDataSourcePropertyToOverride_ManifestFileLocation = AssetBundleExportJobDataSourcePropertyToOverride' "ManifestFileLocation"

pattern AssetBundleExportJobDataSourcePropertyToOverride_Name :: AssetBundleExportJobDataSourcePropertyToOverride
pattern AssetBundleExportJobDataSourcePropertyToOverride_Name = AssetBundleExportJobDataSourcePropertyToOverride' "Name"

pattern AssetBundleExportJobDataSourcePropertyToOverride_Password :: AssetBundleExportJobDataSourcePropertyToOverride
pattern AssetBundleExportJobDataSourcePropertyToOverride_Password = AssetBundleExportJobDataSourcePropertyToOverride' "Password"

pattern AssetBundleExportJobDataSourcePropertyToOverride_Port :: AssetBundleExportJobDataSourcePropertyToOverride
pattern AssetBundleExportJobDataSourcePropertyToOverride_Port = AssetBundleExportJobDataSourcePropertyToOverride' "Port"

pattern AssetBundleExportJobDataSourcePropertyToOverride_RoleArn :: AssetBundleExportJobDataSourcePropertyToOverride
pattern AssetBundleExportJobDataSourcePropertyToOverride_RoleArn = AssetBundleExportJobDataSourcePropertyToOverride' "RoleArn"

pattern AssetBundleExportJobDataSourcePropertyToOverride_SecretArn :: AssetBundleExportJobDataSourcePropertyToOverride
pattern AssetBundleExportJobDataSourcePropertyToOverride_SecretArn = AssetBundleExportJobDataSourcePropertyToOverride' "SecretArn"

pattern AssetBundleExportJobDataSourcePropertyToOverride_Username :: AssetBundleExportJobDataSourcePropertyToOverride
pattern AssetBundleExportJobDataSourcePropertyToOverride_Username = AssetBundleExportJobDataSourcePropertyToOverride' "Username"

pattern AssetBundleExportJobDataSourcePropertyToOverride_Warehouse :: AssetBundleExportJobDataSourcePropertyToOverride
pattern AssetBundleExportJobDataSourcePropertyToOverride_Warehouse = AssetBundleExportJobDataSourcePropertyToOverride' "Warehouse"

pattern AssetBundleExportJobDataSourcePropertyToOverride_WorkGroup :: AssetBundleExportJobDataSourcePropertyToOverride
pattern AssetBundleExportJobDataSourcePropertyToOverride_WorkGroup = AssetBundleExportJobDataSourcePropertyToOverride' "WorkGroup"

{-# COMPLETE
  AssetBundleExportJobDataSourcePropertyToOverride_Catalog,
  AssetBundleExportJobDataSourcePropertyToOverride_ClusterId,
  AssetBundleExportJobDataSourcePropertyToOverride_DataSetName,
  AssetBundleExportJobDataSourcePropertyToOverride_Database,
  AssetBundleExportJobDataSourcePropertyToOverride_DisableSsl,
  AssetBundleExportJobDataSourcePropertyToOverride_Domain,
  AssetBundleExportJobDataSourcePropertyToOverride_Host,
  AssetBundleExportJobDataSourcePropertyToOverride_InstanceId,
  AssetBundleExportJobDataSourcePropertyToOverride_ManifestFileLocation,
  AssetBundleExportJobDataSourcePropertyToOverride_Name,
  AssetBundleExportJobDataSourcePropertyToOverride_Password,
  AssetBundleExportJobDataSourcePropertyToOverride_Port,
  AssetBundleExportJobDataSourcePropertyToOverride_RoleArn,
  AssetBundleExportJobDataSourcePropertyToOverride_SecretArn,
  AssetBundleExportJobDataSourcePropertyToOverride_Username,
  AssetBundleExportJobDataSourcePropertyToOverride_Warehouse,
  AssetBundleExportJobDataSourcePropertyToOverride_WorkGroup,
  AssetBundleExportJobDataSourcePropertyToOverride'
  #-}
