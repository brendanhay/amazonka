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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | A set of alternate data source parameters that you want to share for the
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
    -- | The Amazon Resource Name (ARN) of the data source.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time that this data source was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the data source. This ID is unique per Amazon Web Services
    -- Region for each Amazon Web Services account.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The parameters that Amazon QuickSight uses to connect to your underlying
    -- source. This is a variant type structure. For this structure to be
    -- valid, only one of the attributes can be non-null.
    dataSourceParameters :: Prelude.Maybe DataSourceParameters,
    -- | Error information from the last update or the creation of the data
    -- source.
    errorInfo :: Prelude.Maybe DataSourceErrorInfo,
    -- | The last time that this data source was updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | A display name for the data source.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the secret associated with the data
    -- source in Amazon Secrets Manager.
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | Secure Socket Layer (SSL) properties that apply when Amazon QuickSight
    -- connects to your underlying source.
    sslProperties :: Prelude.Maybe SslProperties,
    -- | The HTTP status of the request.
    status :: Prelude.Maybe ResourceStatus,
    -- | The type of the data source. This type indicates which database engine
    -- the data source connects to.
    type' :: Prelude.Maybe DataSourceType,
    -- | The VPC connection information. You need to use this parameter only when
    -- you want Amazon QuickSight to use a VPC connection when connecting to
    -- your underlying source.
    vpcConnectionProperties :: Prelude.Maybe VpcConnectionProperties
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
-- 'arn', 'dataSource_arn' - The Amazon Resource Name (ARN) of the data source.
--
-- 'createdTime', 'dataSource_createdTime' - The time that this data source was created.
--
-- 'dataSourceId', 'dataSource_dataSourceId' - The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
--
-- 'dataSourceParameters', 'dataSource_dataSourceParameters' - The parameters that Amazon QuickSight uses to connect to your underlying
-- source. This is a variant type structure. For this structure to be
-- valid, only one of the attributes can be non-null.
--
-- 'errorInfo', 'dataSource_errorInfo' - Error information from the last update or the creation of the data
-- source.
--
-- 'lastUpdatedTime', 'dataSource_lastUpdatedTime' - The last time that this data source was updated.
--
-- 'name', 'dataSource_name' - A display name for the data source.
--
-- 'secretArn', 'dataSource_secretArn' - The Amazon Resource Name (ARN) of the secret associated with the data
-- source in Amazon Secrets Manager.
--
-- 'sslProperties', 'dataSource_sslProperties' - Secure Socket Layer (SSL) properties that apply when Amazon QuickSight
-- connects to your underlying source.
--
-- 'status', 'dataSource_status' - The HTTP status of the request.
--
-- 'type'', 'dataSource_type' - The type of the data source. This type indicates which database engine
-- the data source connects to.
--
-- 'vpcConnectionProperties', 'dataSource_vpcConnectionProperties' - The VPC connection information. You need to use this parameter only when
-- you want Amazon QuickSight to use a VPC connection when connecting to
-- your underlying source.
newDataSource ::
  DataSource
newDataSource =
  DataSource'
    { alternateDataSourceParameters =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      dataSourceId = Prelude.Nothing,
      dataSourceParameters = Prelude.Nothing,
      errorInfo = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      secretArn = Prelude.Nothing,
      sslProperties = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing,
      vpcConnectionProperties = Prelude.Nothing
    }

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

-- | The Amazon Resource Name (ARN) of the data source.
dataSource_arn :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_arn = Lens.lens (\DataSource' {arn} -> arn) (\s@DataSource' {} a -> s {arn = a} :: DataSource)

-- | The time that this data source was created.
dataSource_createdTime :: Lens.Lens' DataSource (Prelude.Maybe Prelude.UTCTime)
dataSource_createdTime = Lens.lens (\DataSource' {createdTime} -> createdTime) (\s@DataSource' {} a -> s {createdTime = a} :: DataSource) Prelude.. Lens.mapping Data._Time

-- | The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
dataSource_dataSourceId :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_dataSourceId = Lens.lens (\DataSource' {dataSourceId} -> dataSourceId) (\s@DataSource' {} a -> s {dataSourceId = a} :: DataSource)

-- | The parameters that Amazon QuickSight uses to connect to your underlying
-- source. This is a variant type structure. For this structure to be
-- valid, only one of the attributes can be non-null.
dataSource_dataSourceParameters :: Lens.Lens' DataSource (Prelude.Maybe DataSourceParameters)
dataSource_dataSourceParameters = Lens.lens (\DataSource' {dataSourceParameters} -> dataSourceParameters) (\s@DataSource' {} a -> s {dataSourceParameters = a} :: DataSource)

-- | Error information from the last update or the creation of the data
-- source.
dataSource_errorInfo :: Lens.Lens' DataSource (Prelude.Maybe DataSourceErrorInfo)
dataSource_errorInfo = Lens.lens (\DataSource' {errorInfo} -> errorInfo) (\s@DataSource' {} a -> s {errorInfo = a} :: DataSource)

-- | The last time that this data source was updated.
dataSource_lastUpdatedTime :: Lens.Lens' DataSource (Prelude.Maybe Prelude.UTCTime)
dataSource_lastUpdatedTime = Lens.lens (\DataSource' {lastUpdatedTime} -> lastUpdatedTime) (\s@DataSource' {} a -> s {lastUpdatedTime = a} :: DataSource) Prelude.. Lens.mapping Data._Time

-- | A display name for the data source.
dataSource_name :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_name = Lens.lens (\DataSource' {name} -> name) (\s@DataSource' {} a -> s {name = a} :: DataSource)

-- | The Amazon Resource Name (ARN) of the secret associated with the data
-- source in Amazon Secrets Manager.
dataSource_secretArn :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_secretArn = Lens.lens (\DataSource' {secretArn} -> secretArn) (\s@DataSource' {} a -> s {secretArn = a} :: DataSource)

-- | Secure Socket Layer (SSL) properties that apply when Amazon QuickSight
-- connects to your underlying source.
dataSource_sslProperties :: Lens.Lens' DataSource (Prelude.Maybe SslProperties)
dataSource_sslProperties = Lens.lens (\DataSource' {sslProperties} -> sslProperties) (\s@DataSource' {} a -> s {sslProperties = a} :: DataSource)

-- | The HTTP status of the request.
dataSource_status :: Lens.Lens' DataSource (Prelude.Maybe ResourceStatus)
dataSource_status = Lens.lens (\DataSource' {status} -> status) (\s@DataSource' {} a -> s {status = a} :: DataSource)

-- | The type of the data source. This type indicates which database engine
-- the data source connects to.
dataSource_type :: Lens.Lens' DataSource (Prelude.Maybe DataSourceType)
dataSource_type = Lens.lens (\DataSource' {type'} -> type') (\s@DataSource' {} a -> s {type' = a} :: DataSource)

-- | The VPC connection information. You need to use this parameter only when
-- you want Amazon QuickSight to use a VPC connection when connecting to
-- your underlying source.
dataSource_vpcConnectionProperties :: Lens.Lens' DataSource (Prelude.Maybe VpcConnectionProperties)
dataSource_vpcConnectionProperties = Lens.lens (\DataSource' {vpcConnectionProperties} -> vpcConnectionProperties) (\s@DataSource' {} a -> s {vpcConnectionProperties = a} :: DataSource)

instance Data.FromJSON DataSource where
  parseJSON =
    Data.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Prelude.<$> (x Data..:? "AlternateDataSourceParameters")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "DataSourceId")
            Prelude.<*> (x Data..:? "DataSourceParameters")
            Prelude.<*> (x Data..:? "ErrorInfo")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "SecretArn")
            Prelude.<*> (x Data..:? "SslProperties")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "VpcConnectionProperties")
      )

instance Prelude.Hashable DataSource where
  hashWithSalt _salt DataSource' {..} =
    _salt
      `Prelude.hashWithSalt` alternateDataSourceParameters
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` dataSourceId
      `Prelude.hashWithSalt` dataSourceParameters
      `Prelude.hashWithSalt` errorInfo
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` sslProperties
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` vpcConnectionProperties

instance Prelude.NFData DataSource where
  rnf DataSource' {..} =
    Prelude.rnf alternateDataSourceParameters `Prelude.seq`
      Prelude.rnf arn `Prelude.seq`
        Prelude.rnf createdTime `Prelude.seq`
          Prelude.rnf dataSourceId `Prelude.seq`
            Prelude.rnf dataSourceParameters `Prelude.seq`
              Prelude.rnf errorInfo `Prelude.seq`
                Prelude.rnf lastUpdatedTime `Prelude.seq`
                  Prelude.rnf name `Prelude.seq`
                    Prelude.rnf secretArn `Prelude.seq`
                      Prelude.rnf sslProperties `Prelude.seq`
                        Prelude.rnf status `Prelude.seq`
                          Prelude.rnf type' `Prelude.seq`
                            Prelude.rnf vpcConnectionProperties
