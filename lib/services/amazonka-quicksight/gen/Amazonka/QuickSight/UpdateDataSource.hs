{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.QuickSight.UpdateDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a data source.
module Amazonka.QuickSight.UpdateDataSource
  ( -- * Creating a Request
    UpdateDataSource (..),
    newUpdateDataSource,

    -- * Request Lenses
    updateDataSource_credentials,
    updateDataSource_dataSourceParameters,
    updateDataSource_sslProperties,
    updateDataSource_vpcConnectionProperties,
    updateDataSource_awsAccountId,
    updateDataSource_dataSourceId,
    updateDataSource_name,

    -- * Destructuring the Response
    UpdateDataSourceResponse (..),
    newUpdateDataSourceResponse,

    -- * Response Lenses
    updateDataSourceResponse_arn,
    updateDataSourceResponse_dataSourceId,
    updateDataSourceResponse_requestId,
    updateDataSourceResponse_updateStatus,
    updateDataSourceResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDataSource' smart constructor.
data UpdateDataSource = UpdateDataSource'
  { -- | The credentials that Amazon QuickSight that uses to connect to your
    -- underlying source. Currently, only credentials based on user name and
    -- password are supported.
    credentials :: Prelude.Maybe (Data.Sensitive DataSourceCredentials),
    -- | The parameters that Amazon QuickSight uses to connect to your underlying
    -- source.
    dataSourceParameters :: Prelude.Maybe DataSourceParameters,
    -- | Secure Socket Layer (SSL) properties that apply when Amazon QuickSight
    -- connects to your underlying source.
    sslProperties :: Prelude.Maybe SslProperties,
    -- | Use this parameter only when you want Amazon QuickSight to use a VPC
    -- connection when connecting to your underlying source.
    vpcConnectionProperties :: Prelude.Maybe VpcConnectionProperties,
    -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID of the data source. This ID is unique per Amazon Web Services
    -- Region for each Amazon Web Services account.
    dataSourceId :: Prelude.Text,
    -- | A display name for the data source.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentials', 'updateDataSource_credentials' - The credentials that Amazon QuickSight that uses to connect to your
-- underlying source. Currently, only credentials based on user name and
-- password are supported.
--
-- 'dataSourceParameters', 'updateDataSource_dataSourceParameters' - The parameters that Amazon QuickSight uses to connect to your underlying
-- source.
--
-- 'sslProperties', 'updateDataSource_sslProperties' - Secure Socket Layer (SSL) properties that apply when Amazon QuickSight
-- connects to your underlying source.
--
-- 'vpcConnectionProperties', 'updateDataSource_vpcConnectionProperties' - Use this parameter only when you want Amazon QuickSight to use a VPC
-- connection when connecting to your underlying source.
--
-- 'awsAccountId', 'updateDataSource_awsAccountId' - The Amazon Web Services account ID.
--
-- 'dataSourceId', 'updateDataSource_dataSourceId' - The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
--
-- 'name', 'updateDataSource_name' - A display name for the data source.
newUpdateDataSource ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dataSourceId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  UpdateDataSource
newUpdateDataSource
  pAwsAccountId_
  pDataSourceId_
  pName_ =
    UpdateDataSource'
      { credentials = Prelude.Nothing,
        dataSourceParameters = Prelude.Nothing,
        sslProperties = Prelude.Nothing,
        vpcConnectionProperties = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        dataSourceId = pDataSourceId_,
        name = pName_
      }

-- | The credentials that Amazon QuickSight that uses to connect to your
-- underlying source. Currently, only credentials based on user name and
-- password are supported.
updateDataSource_credentials :: Lens.Lens' UpdateDataSource (Prelude.Maybe DataSourceCredentials)
updateDataSource_credentials = Lens.lens (\UpdateDataSource' {credentials} -> credentials) (\s@UpdateDataSource' {} a -> s {credentials = a} :: UpdateDataSource) Prelude.. Lens.mapping Data._Sensitive

-- | The parameters that Amazon QuickSight uses to connect to your underlying
-- source.
updateDataSource_dataSourceParameters :: Lens.Lens' UpdateDataSource (Prelude.Maybe DataSourceParameters)
updateDataSource_dataSourceParameters = Lens.lens (\UpdateDataSource' {dataSourceParameters} -> dataSourceParameters) (\s@UpdateDataSource' {} a -> s {dataSourceParameters = a} :: UpdateDataSource)

-- | Secure Socket Layer (SSL) properties that apply when Amazon QuickSight
-- connects to your underlying source.
updateDataSource_sslProperties :: Lens.Lens' UpdateDataSource (Prelude.Maybe SslProperties)
updateDataSource_sslProperties = Lens.lens (\UpdateDataSource' {sslProperties} -> sslProperties) (\s@UpdateDataSource' {} a -> s {sslProperties = a} :: UpdateDataSource)

-- | Use this parameter only when you want Amazon QuickSight to use a VPC
-- connection when connecting to your underlying source.
updateDataSource_vpcConnectionProperties :: Lens.Lens' UpdateDataSource (Prelude.Maybe VpcConnectionProperties)
updateDataSource_vpcConnectionProperties = Lens.lens (\UpdateDataSource' {vpcConnectionProperties} -> vpcConnectionProperties) (\s@UpdateDataSource' {} a -> s {vpcConnectionProperties = a} :: UpdateDataSource)

-- | The Amazon Web Services account ID.
updateDataSource_awsAccountId :: Lens.Lens' UpdateDataSource Prelude.Text
updateDataSource_awsAccountId = Lens.lens (\UpdateDataSource' {awsAccountId} -> awsAccountId) (\s@UpdateDataSource' {} a -> s {awsAccountId = a} :: UpdateDataSource)

-- | The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
updateDataSource_dataSourceId :: Lens.Lens' UpdateDataSource Prelude.Text
updateDataSource_dataSourceId = Lens.lens (\UpdateDataSource' {dataSourceId} -> dataSourceId) (\s@UpdateDataSource' {} a -> s {dataSourceId = a} :: UpdateDataSource)

-- | A display name for the data source.
updateDataSource_name :: Lens.Lens' UpdateDataSource Prelude.Text
updateDataSource_name = Lens.lens (\UpdateDataSource' {name} -> name) (\s@UpdateDataSource' {} a -> s {name = a} :: UpdateDataSource)

instance Core.AWSRequest UpdateDataSource where
  type
    AWSResponse UpdateDataSource =
      UpdateDataSourceResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDataSourceResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "DataSourceId")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "UpdateStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDataSource where
  hashWithSalt _salt UpdateDataSource' {..} =
    _salt
      `Prelude.hashWithSalt` credentials
      `Prelude.hashWithSalt` dataSourceParameters
      `Prelude.hashWithSalt` sslProperties
      `Prelude.hashWithSalt` vpcConnectionProperties
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dataSourceId
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateDataSource where
  rnf UpdateDataSource' {..} =
    Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf dataSourceParameters
      `Prelude.seq` Prelude.rnf sslProperties
      `Prelude.seq` Prelude.rnf vpcConnectionProperties
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dataSourceId
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateDataSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDataSource where
  toJSON UpdateDataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Credentials" Data..=) Prelude.<$> credentials,
            ("DataSourceParameters" Data..=)
              Prelude.<$> dataSourceParameters,
            ("SslProperties" Data..=) Prelude.<$> sslProperties,
            ("VpcConnectionProperties" Data..=)
              Prelude.<$> vpcConnectionProperties,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath UpdateDataSource where
  toPath UpdateDataSource' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sources/",
        Data.toBS dataSourceId
      ]

instance Data.ToQuery UpdateDataSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDataSourceResponse' smart constructor.
data UpdateDataSourceResponse = UpdateDataSourceResponse'
  { -- | The Amazon Resource Name (ARN) of the data source.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the data source. This ID is unique per Amazon Web Services
    -- Region for each Amazon Web Services account.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The update status of the data source\'s last update.
    updateStatus :: Prelude.Maybe ResourceStatus,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateDataSourceResponse_arn' - The Amazon Resource Name (ARN) of the data source.
--
-- 'dataSourceId', 'updateDataSourceResponse_dataSourceId' - The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
--
-- 'requestId', 'updateDataSourceResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'updateStatus', 'updateDataSourceResponse_updateStatus' - The update status of the data source\'s last update.
--
-- 'status', 'updateDataSourceResponse_status' - The HTTP status of the request.
newUpdateDataSourceResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateDataSourceResponse
newUpdateDataSourceResponse pStatus_ =
  UpdateDataSourceResponse'
    { arn = Prelude.Nothing,
      dataSourceId = Prelude.Nothing,
      requestId = Prelude.Nothing,
      updateStatus = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the data source.
updateDataSourceResponse_arn :: Lens.Lens' UpdateDataSourceResponse (Prelude.Maybe Prelude.Text)
updateDataSourceResponse_arn = Lens.lens (\UpdateDataSourceResponse' {arn} -> arn) (\s@UpdateDataSourceResponse' {} a -> s {arn = a} :: UpdateDataSourceResponse)

-- | The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
updateDataSourceResponse_dataSourceId :: Lens.Lens' UpdateDataSourceResponse (Prelude.Maybe Prelude.Text)
updateDataSourceResponse_dataSourceId = Lens.lens (\UpdateDataSourceResponse' {dataSourceId} -> dataSourceId) (\s@UpdateDataSourceResponse' {} a -> s {dataSourceId = a} :: UpdateDataSourceResponse)

-- | The Amazon Web Services request ID for this operation.
updateDataSourceResponse_requestId :: Lens.Lens' UpdateDataSourceResponse (Prelude.Maybe Prelude.Text)
updateDataSourceResponse_requestId = Lens.lens (\UpdateDataSourceResponse' {requestId} -> requestId) (\s@UpdateDataSourceResponse' {} a -> s {requestId = a} :: UpdateDataSourceResponse)

-- | The update status of the data source\'s last update.
updateDataSourceResponse_updateStatus :: Lens.Lens' UpdateDataSourceResponse (Prelude.Maybe ResourceStatus)
updateDataSourceResponse_updateStatus = Lens.lens (\UpdateDataSourceResponse' {updateStatus} -> updateStatus) (\s@UpdateDataSourceResponse' {} a -> s {updateStatus = a} :: UpdateDataSourceResponse)

-- | The HTTP status of the request.
updateDataSourceResponse_status :: Lens.Lens' UpdateDataSourceResponse Prelude.Int
updateDataSourceResponse_status = Lens.lens (\UpdateDataSourceResponse' {status} -> status) (\s@UpdateDataSourceResponse' {} a -> s {status = a} :: UpdateDataSourceResponse)

instance Prelude.NFData UpdateDataSourceResponse where
  rnf UpdateDataSourceResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf dataSourceId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf updateStatus
      `Prelude.seq` Prelude.rnf status
