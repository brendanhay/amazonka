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
-- Module      : Amazonka.QuickSight.CreateDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a data source.
module Amazonka.QuickSight.CreateDataSource
  ( -- * Creating a Request
    CreateDataSource (..),
    newCreateDataSource,

    -- * Request Lenses
    createDataSource_credentials,
    createDataSource_dataSourceParameters,
    createDataSource_permissions,
    createDataSource_sslProperties,
    createDataSource_tags,
    createDataSource_vpcConnectionProperties,
    createDataSource_awsAccountId,
    createDataSource_dataSourceId,
    createDataSource_name,
    createDataSource_type,

    -- * Destructuring the Response
    CreateDataSourceResponse (..),
    newCreateDataSourceResponse,

    -- * Response Lenses
    createDataSourceResponse_arn,
    createDataSourceResponse_creationStatus,
    createDataSourceResponse_dataSourceId,
    createDataSourceResponse_requestId,
    createDataSourceResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDataSource' smart constructor.
data CreateDataSource = CreateDataSource'
  { -- | The credentials Amazon QuickSight that uses to connect to your
    -- underlying source. Currently, only credentials based on user name and
    -- password are supported.
    credentials :: Prelude.Maybe (Data.Sensitive DataSourceCredentials),
    -- | The parameters that Amazon QuickSight uses to connect to your underlying
    -- source.
    dataSourceParameters :: Prelude.Maybe DataSourceParameters,
    -- | A list of resource permissions on the data source.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | Secure Socket Layer (SSL) properties that apply when Amazon QuickSight
    -- connects to your underlying source.
    sslProperties :: Prelude.Maybe SslProperties,
    -- | Contains a map of the key-value pairs for the resource tag or tags
    -- assigned to the data source.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Use this parameter only when you want Amazon QuickSight to use a VPC
    -- connection when connecting to your underlying source.
    vpcConnectionProperties :: Prelude.Maybe VpcConnectionProperties,
    -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | An ID for the data source. This ID is unique per Amazon Web Services
    -- Region for each Amazon Web Services account.
    dataSourceId :: Prelude.Text,
    -- | A display name for the data source.
    name :: Prelude.Text,
    -- | The type of the data source. To return a list of all data sources, use
    -- @ListDataSources@.
    --
    -- Use @AMAZON_ELASTICSEARCH@ for Amazon OpenSearch Service.
    type' :: DataSourceType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentials', 'createDataSource_credentials' - The credentials Amazon QuickSight that uses to connect to your
-- underlying source. Currently, only credentials based on user name and
-- password are supported.
--
-- 'dataSourceParameters', 'createDataSource_dataSourceParameters' - The parameters that Amazon QuickSight uses to connect to your underlying
-- source.
--
-- 'permissions', 'createDataSource_permissions' - A list of resource permissions on the data source.
--
-- 'sslProperties', 'createDataSource_sslProperties' - Secure Socket Layer (SSL) properties that apply when Amazon QuickSight
-- connects to your underlying source.
--
-- 'tags', 'createDataSource_tags' - Contains a map of the key-value pairs for the resource tag or tags
-- assigned to the data source.
--
-- 'vpcConnectionProperties', 'createDataSource_vpcConnectionProperties' - Use this parameter only when you want Amazon QuickSight to use a VPC
-- connection when connecting to your underlying source.
--
-- 'awsAccountId', 'createDataSource_awsAccountId' - The Amazon Web Services account ID.
--
-- 'dataSourceId', 'createDataSource_dataSourceId' - An ID for the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
--
-- 'name', 'createDataSource_name' - A display name for the data source.
--
-- 'type'', 'createDataSource_type' - The type of the data source. To return a list of all data sources, use
-- @ListDataSources@.
--
-- Use @AMAZON_ELASTICSEARCH@ for Amazon OpenSearch Service.
newCreateDataSource ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dataSourceId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  DataSourceType ->
  CreateDataSource
newCreateDataSource
  pAwsAccountId_
  pDataSourceId_
  pName_
  pType_ =
    CreateDataSource'
      { credentials = Prelude.Nothing,
        dataSourceParameters = Prelude.Nothing,
        permissions = Prelude.Nothing,
        sslProperties = Prelude.Nothing,
        tags = Prelude.Nothing,
        vpcConnectionProperties = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        dataSourceId = pDataSourceId_,
        name = pName_,
        type' = pType_
      }

-- | The credentials Amazon QuickSight that uses to connect to your
-- underlying source. Currently, only credentials based on user name and
-- password are supported.
createDataSource_credentials :: Lens.Lens' CreateDataSource (Prelude.Maybe DataSourceCredentials)
createDataSource_credentials = Lens.lens (\CreateDataSource' {credentials} -> credentials) (\s@CreateDataSource' {} a -> s {credentials = a} :: CreateDataSource) Prelude.. Lens.mapping Data._Sensitive

-- | The parameters that Amazon QuickSight uses to connect to your underlying
-- source.
createDataSource_dataSourceParameters :: Lens.Lens' CreateDataSource (Prelude.Maybe DataSourceParameters)
createDataSource_dataSourceParameters = Lens.lens (\CreateDataSource' {dataSourceParameters} -> dataSourceParameters) (\s@CreateDataSource' {} a -> s {dataSourceParameters = a} :: CreateDataSource)

-- | A list of resource permissions on the data source.
createDataSource_permissions :: Lens.Lens' CreateDataSource (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
createDataSource_permissions = Lens.lens (\CreateDataSource' {permissions} -> permissions) (\s@CreateDataSource' {} a -> s {permissions = a} :: CreateDataSource) Prelude.. Lens.mapping Lens.coerced

-- | Secure Socket Layer (SSL) properties that apply when Amazon QuickSight
-- connects to your underlying source.
createDataSource_sslProperties :: Lens.Lens' CreateDataSource (Prelude.Maybe SslProperties)
createDataSource_sslProperties = Lens.lens (\CreateDataSource' {sslProperties} -> sslProperties) (\s@CreateDataSource' {} a -> s {sslProperties = a} :: CreateDataSource)

-- | Contains a map of the key-value pairs for the resource tag or tags
-- assigned to the data source.
createDataSource_tags :: Lens.Lens' CreateDataSource (Prelude.Maybe (Prelude.NonEmpty Tag))
createDataSource_tags = Lens.lens (\CreateDataSource' {tags} -> tags) (\s@CreateDataSource' {} a -> s {tags = a} :: CreateDataSource) Prelude.. Lens.mapping Lens.coerced

-- | Use this parameter only when you want Amazon QuickSight to use a VPC
-- connection when connecting to your underlying source.
createDataSource_vpcConnectionProperties :: Lens.Lens' CreateDataSource (Prelude.Maybe VpcConnectionProperties)
createDataSource_vpcConnectionProperties = Lens.lens (\CreateDataSource' {vpcConnectionProperties} -> vpcConnectionProperties) (\s@CreateDataSource' {} a -> s {vpcConnectionProperties = a} :: CreateDataSource)

-- | The Amazon Web Services account ID.
createDataSource_awsAccountId :: Lens.Lens' CreateDataSource Prelude.Text
createDataSource_awsAccountId = Lens.lens (\CreateDataSource' {awsAccountId} -> awsAccountId) (\s@CreateDataSource' {} a -> s {awsAccountId = a} :: CreateDataSource)

-- | An ID for the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
createDataSource_dataSourceId :: Lens.Lens' CreateDataSource Prelude.Text
createDataSource_dataSourceId = Lens.lens (\CreateDataSource' {dataSourceId} -> dataSourceId) (\s@CreateDataSource' {} a -> s {dataSourceId = a} :: CreateDataSource)

-- | A display name for the data source.
createDataSource_name :: Lens.Lens' CreateDataSource Prelude.Text
createDataSource_name = Lens.lens (\CreateDataSource' {name} -> name) (\s@CreateDataSource' {} a -> s {name = a} :: CreateDataSource)

-- | The type of the data source. To return a list of all data sources, use
-- @ListDataSources@.
--
-- Use @AMAZON_ELASTICSEARCH@ for Amazon OpenSearch Service.
createDataSource_type :: Lens.Lens' CreateDataSource DataSourceType
createDataSource_type = Lens.lens (\CreateDataSource' {type'} -> type') (\s@CreateDataSource' {} a -> s {type' = a} :: CreateDataSource)

instance Core.AWSRequest CreateDataSource where
  type
    AWSResponse CreateDataSource =
      CreateDataSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataSourceResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationStatus")
            Prelude.<*> (x Data..?> "DataSourceId")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataSource where
  hashWithSalt _salt CreateDataSource' {..} =
    _salt
      `Prelude.hashWithSalt` credentials
      `Prelude.hashWithSalt` dataSourceParameters
      `Prelude.hashWithSalt` permissions
      `Prelude.hashWithSalt` sslProperties
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcConnectionProperties
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dataSourceId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateDataSource where
  rnf CreateDataSource' {..} =
    Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf dataSourceParameters
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf sslProperties
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcConnectionProperties
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dataSourceId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders CreateDataSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDataSource where
  toJSON CreateDataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Credentials" Data..=) Prelude.<$> credentials,
            ("DataSourceParameters" Data..=)
              Prelude.<$> dataSourceParameters,
            ("Permissions" Data..=) Prelude.<$> permissions,
            ("SslProperties" Data..=) Prelude.<$> sslProperties,
            ("Tags" Data..=) Prelude.<$> tags,
            ("VpcConnectionProperties" Data..=)
              Prelude.<$> vpcConnectionProperties,
            Prelude.Just ("DataSourceId" Data..= dataSourceId),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Type" Data..= type')
          ]
      )

instance Data.ToPath CreateDataSource where
  toPath CreateDataSource' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sources"
      ]

instance Data.ToQuery CreateDataSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDataSourceResponse' smart constructor.
data CreateDataSourceResponse = CreateDataSourceResponse'
  { -- | The Amazon Resource Name (ARN) of the data source.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of creating the data source.
    creationStatus :: Prelude.Maybe ResourceStatus,
    -- | The ID of the data source. This ID is unique per Amazon Web Services
    -- Region for each Amazon Web Services account.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createDataSourceResponse_arn' - The Amazon Resource Name (ARN) of the data source.
--
-- 'creationStatus', 'createDataSourceResponse_creationStatus' - The status of creating the data source.
--
-- 'dataSourceId', 'createDataSourceResponse_dataSourceId' - The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
--
-- 'requestId', 'createDataSourceResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'createDataSourceResponse_status' - The HTTP status of the request.
newCreateDataSourceResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateDataSourceResponse
newCreateDataSourceResponse pStatus_ =
  CreateDataSourceResponse'
    { arn = Prelude.Nothing,
      creationStatus = Prelude.Nothing,
      dataSourceId = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the data source.
createDataSourceResponse_arn :: Lens.Lens' CreateDataSourceResponse (Prelude.Maybe Prelude.Text)
createDataSourceResponse_arn = Lens.lens (\CreateDataSourceResponse' {arn} -> arn) (\s@CreateDataSourceResponse' {} a -> s {arn = a} :: CreateDataSourceResponse)

-- | The status of creating the data source.
createDataSourceResponse_creationStatus :: Lens.Lens' CreateDataSourceResponse (Prelude.Maybe ResourceStatus)
createDataSourceResponse_creationStatus = Lens.lens (\CreateDataSourceResponse' {creationStatus} -> creationStatus) (\s@CreateDataSourceResponse' {} a -> s {creationStatus = a} :: CreateDataSourceResponse)

-- | The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
createDataSourceResponse_dataSourceId :: Lens.Lens' CreateDataSourceResponse (Prelude.Maybe Prelude.Text)
createDataSourceResponse_dataSourceId = Lens.lens (\CreateDataSourceResponse' {dataSourceId} -> dataSourceId) (\s@CreateDataSourceResponse' {} a -> s {dataSourceId = a} :: CreateDataSourceResponse)

-- | The Amazon Web Services request ID for this operation.
createDataSourceResponse_requestId :: Lens.Lens' CreateDataSourceResponse (Prelude.Maybe Prelude.Text)
createDataSourceResponse_requestId = Lens.lens (\CreateDataSourceResponse' {requestId} -> requestId) (\s@CreateDataSourceResponse' {} a -> s {requestId = a} :: CreateDataSourceResponse)

-- | The HTTP status of the request.
createDataSourceResponse_status :: Lens.Lens' CreateDataSourceResponse Prelude.Int
createDataSourceResponse_status = Lens.lens (\CreateDataSourceResponse' {status} -> status) (\s@CreateDataSourceResponse' {} a -> s {status = a} :: CreateDataSourceResponse)

instance Prelude.NFData CreateDataSourceResponse where
  rnf CreateDataSourceResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationStatus
      `Prelude.seq` Prelude.rnf dataSourceId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
