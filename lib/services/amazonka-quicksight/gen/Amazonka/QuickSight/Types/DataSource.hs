{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.QuickSight.Types.DataSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataSourceErrorInfo
import Amazonka.QuickSight.Types.DataSourceParameters
import Amazonka.QuickSight.Types.DataSourceType
import Amazonka.QuickSight.Types.ResourceStatus
import Amazonka.QuickSight.Types.SslProperties
import Amazonka.QuickSight.Types.VpcConnectionProperties

-- | The structure of a data source.
--
-- /See:/ 'newDataSource' smart constructor.
data DataSource = DataSource'
  { -- | A display name for the data source.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the data source. This type indicates which database engine
    -- the data source connects to.
    type' :: Prelude.Maybe DataSourceType,
    -- | The ID of the data source. This ID is unique per Amazon Web Services
    -- Region for each Amazon Web Services account.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The time that this data source was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The parameters that Amazon QuickSight uses to connect to your underlying
    -- source. This is a variant type structure. For this structure to be
    -- valid, only one of the attributes can be non-null.
    dataSourceParameters :: Prelude.Maybe DataSourceParameters,
    -- | The Amazon Resource Name (ARN) of the data source.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Maybe ResourceStatus,
    -- | The last time that this data source was updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The VPC connection information. You need to use this parameter only when
    -- you want Amazon QuickSight to use a VPC connection when connecting to
    -- your underlying source.
    vpcConnectionProperties :: Prelude.Maybe VpcConnectionProperties,
    -- | Secure Socket Layer (SSL) properties that apply when Amazon QuickSight
    -- connects to your underlying source.
    sslProperties :: Prelude.Maybe SslProperties,
    -- | The Amazon Resource Name (ARN) of the secret associated with the data
    -- source in Amazon Secrets Manager.
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | A set of alternate data source parameters that you want to share for the
    -- credentials stored with this data source. The credentials are applied in
    -- tandem with the data source parameters when you copy a data source by
    -- using a create or update request. The API operation compares the
    -- @DataSourceParameters@ structure that\'s in the request with the
    -- structures in the @AlternateDataSourceParameters@ allow list. If the
    -- structures are an exact match, the request is allowed to use the
    -- credentials from this existing data source. If the
    -- @AlternateDataSourceParameters@ list is null, the @Credentials@
    -- originally used with this @DataSourceParameters@ are automatically
    -- allowed.
    alternateDataSourceParameters :: Prelude.Maybe (Prelude.NonEmpty DataSourceParameters),
    -- | Error information from the last update or the creation of the data
    -- source.
    errorInfo :: Prelude.Maybe DataSourceErrorInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'dataSource_name' - A display name for the data source.
--
-- 'type'', 'dataSource_type' - The type of the data source. This type indicates which database engine
-- the data source connects to.
--
-- 'dataSourceId', 'dataSource_dataSourceId' - The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
--
-- 'createdTime', 'dataSource_createdTime' - The time that this data source was created.
--
-- 'dataSourceParameters', 'dataSource_dataSourceParameters' - The parameters that Amazon QuickSight uses to connect to your underlying
-- source. This is a variant type structure. For this structure to be
-- valid, only one of the attributes can be non-null.
--
-- 'arn', 'dataSource_arn' - The Amazon Resource Name (ARN) of the data source.
--
-- 'status', 'dataSource_status' - The HTTP status of the request.
--
-- 'lastUpdatedTime', 'dataSource_lastUpdatedTime' - The last time that this data source was updated.
--
-- 'vpcConnectionProperties', 'dataSource_vpcConnectionProperties' - The VPC connection information. You need to use this parameter only when
-- you want Amazon QuickSight to use a VPC connection when connecting to
-- your underlying source.
--
-- 'sslProperties', 'dataSource_sslProperties' - Secure Socket Layer (SSL) properties that apply when Amazon QuickSight
-- connects to your underlying source.
--
-- 'secretArn', 'dataSource_secretArn' - The Amazon Resource Name (ARN) of the secret associated with the data
-- source in Amazon Secrets Manager.
--
-- 'alternateDataSourceParameters', 'dataSource_alternateDataSourceParameters' - A set of alternate data source parameters that you want to share for the
-- credentials stored with this data source. The credentials are applied in
-- tandem with the data source parameters when you copy a data source by
-- using a create or update request. The API operation compares the
-- @DataSourceParameters@ structure that\'s in the request with the
-- structures in the @AlternateDataSourceParameters@ allow list. If the
-- structures are an exact match, the request is allowed to use the
-- credentials from this existing data source. If the
-- @AlternateDataSourceParameters@ list is null, the @Credentials@
-- originally used with this @DataSourceParameters@ are automatically
-- allowed.
--
-- 'errorInfo', 'dataSource_errorInfo' - Error information from the last update or the creation of the data
-- source.
newDataSource ::
  DataSource
newDataSource =
  DataSource'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      dataSourceId = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      dataSourceParameters = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      vpcConnectionProperties = Prelude.Nothing,
      sslProperties = Prelude.Nothing,
      secretArn = Prelude.Nothing,
      alternateDataSourceParameters = Prelude.Nothing,
      errorInfo = Prelude.Nothing
    }

-- | A display name for the data source.
dataSource_name :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_name = Lens.lens (\DataSource' {name} -> name) (\s@DataSource' {} a -> s {name = a} :: DataSource)

-- | The type of the data source. This type indicates which database engine
-- the data source connects to.
dataSource_type :: Lens.Lens' DataSource (Prelude.Maybe DataSourceType)
dataSource_type = Lens.lens (\DataSource' {type'} -> type') (\s@DataSource' {} a -> s {type' = a} :: DataSource)

-- | The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
dataSource_dataSourceId :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_dataSourceId = Lens.lens (\DataSource' {dataSourceId} -> dataSourceId) (\s@DataSource' {} a -> s {dataSourceId = a} :: DataSource)

-- | The time that this data source was created.
dataSource_createdTime :: Lens.Lens' DataSource (Prelude.Maybe Prelude.UTCTime)
dataSource_createdTime = Lens.lens (\DataSource' {createdTime} -> createdTime) (\s@DataSource' {} a -> s {createdTime = a} :: DataSource) Prelude.. Lens.mapping Core._Time

-- | The parameters that Amazon QuickSight uses to connect to your underlying
-- source. This is a variant type structure. For this structure to be
-- valid, only one of the attributes can be non-null.
dataSource_dataSourceParameters :: Lens.Lens' DataSource (Prelude.Maybe DataSourceParameters)
dataSource_dataSourceParameters = Lens.lens (\DataSource' {dataSourceParameters} -> dataSourceParameters) (\s@DataSource' {} a -> s {dataSourceParameters = a} :: DataSource)

-- | The Amazon Resource Name (ARN) of the data source.
dataSource_arn :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_arn = Lens.lens (\DataSource' {arn} -> arn) (\s@DataSource' {} a -> s {arn = a} :: DataSource)

-- | The HTTP status of the request.
dataSource_status :: Lens.Lens' DataSource (Prelude.Maybe ResourceStatus)
dataSource_status = Lens.lens (\DataSource' {status} -> status) (\s@DataSource' {} a -> s {status = a} :: DataSource)

-- | The last time that this data source was updated.
dataSource_lastUpdatedTime :: Lens.Lens' DataSource (Prelude.Maybe Prelude.UTCTime)
dataSource_lastUpdatedTime = Lens.lens (\DataSource' {lastUpdatedTime} -> lastUpdatedTime) (\s@DataSource' {} a -> s {lastUpdatedTime = a} :: DataSource) Prelude.. Lens.mapping Core._Time

-- | The VPC connection information. You need to use this parameter only when
-- you want Amazon QuickSight to use a VPC connection when connecting to
-- your underlying source.
dataSource_vpcConnectionProperties :: Lens.Lens' DataSource (Prelude.Maybe VpcConnectionProperties)
dataSource_vpcConnectionProperties = Lens.lens (\DataSource' {vpcConnectionProperties} -> vpcConnectionProperties) (\s@DataSource' {} a -> s {vpcConnectionProperties = a} :: DataSource)

-- | Secure Socket Layer (SSL) properties that apply when Amazon QuickSight
-- connects to your underlying source.
dataSource_sslProperties :: Lens.Lens' DataSource (Prelude.Maybe SslProperties)
dataSource_sslProperties = Lens.lens (\DataSource' {sslProperties} -> sslProperties) (\s@DataSource' {} a -> s {sslProperties = a} :: DataSource)

-- | The Amazon Resource Name (ARN) of the secret associated with the data
-- source in Amazon Secrets Manager.
dataSource_secretArn :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_secretArn = Lens.lens (\DataSource' {secretArn} -> secretArn) (\s@DataSource' {} a -> s {secretArn = a} :: DataSource)

-- | A set of alternate data source parameters that you want to share for the
-- credentials stored with this data source. The credentials are applied in
-- tandem with the data source parameters when you copy a data source by
-- using a create or update request. The API operation compares the
-- @DataSourceParameters@ structure that\'s in the request with the
-- structures in the @AlternateDataSourceParameters@ allow list. If the
-- structures are an exact match, the request is allowed to use the
-- credentials from this existing data source. If the
-- @AlternateDataSourceParameters@ list is null, the @Credentials@
-- originally used with this @DataSourceParameters@ are automatically
-- allowed.
dataSource_alternateDataSourceParameters :: Lens.Lens' DataSource (Prelude.Maybe (Prelude.NonEmpty DataSourceParameters))
dataSource_alternateDataSourceParameters = Lens.lens (\DataSource' {alternateDataSourceParameters} -> alternateDataSourceParameters) (\s@DataSource' {} a -> s {alternateDataSourceParameters = a} :: DataSource) Prelude.. Lens.mapping Lens.coerced

-- | Error information from the last update or the creation of the data
-- source.
dataSource_errorInfo :: Lens.Lens' DataSource (Prelude.Maybe DataSourceErrorInfo)
dataSource_errorInfo = Lens.lens (\DataSource' {errorInfo} -> errorInfo) (\s@DataSource' {} a -> s {errorInfo = a} :: DataSource)

instance Core.FromJSON DataSource where
  parseJSON =
    Core.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "DataSourceId")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "DataSourceParameters")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "VpcConnectionProperties")
            Prelude.<*> (x Core..:? "SslProperties")
            Prelude.<*> (x Core..:? "SecretArn")
            Prelude.<*> (x Core..:? "AlternateDataSourceParameters")
            Prelude.<*> (x Core..:? "ErrorInfo")
      )

instance Prelude.Hashable DataSource where
  hashWithSalt _salt DataSource' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` dataSourceId
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` dataSourceParameters
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` vpcConnectionProperties
      `Prelude.hashWithSalt` sslProperties
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` alternateDataSourceParameters
      `Prelude.hashWithSalt` errorInfo

instance Prelude.NFData DataSource where
  rnf DataSource' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf dataSourceId
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf dataSourceParameters
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf vpcConnectionProperties
      `Prelude.seq` Prelude.rnf sslProperties
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf alternateDataSourceParameters
      `Prelude.seq` Prelude.rnf errorInfo
