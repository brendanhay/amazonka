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
-- Module      : Amazonka.HealthLake.CreateFHIRDatastore
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Data Store that can ingest and export FHIR formatted data.
module Amazonka.HealthLake.CreateFHIRDatastore
  ( -- * Creating a Request
    CreateFHIRDatastore (..),
    newCreateFHIRDatastore,

    -- * Request Lenses
    createFHIRDatastore_clientToken,
    createFHIRDatastore_datastoreName,
    createFHIRDatastore_preloadDataConfig,
    createFHIRDatastore_sseConfiguration,
    createFHIRDatastore_tags,
    createFHIRDatastore_datastoreTypeVersion,

    -- * Destructuring the Response
    CreateFHIRDatastoreResponse (..),
    newCreateFHIRDatastoreResponse,

    -- * Response Lenses
    createFHIRDatastoreResponse_httpStatus,
    createFHIRDatastoreResponse_datastoreId,
    createFHIRDatastoreResponse_datastoreArn,
    createFHIRDatastoreResponse_datastoreStatus,
    createFHIRDatastoreResponse_datastoreEndpoint,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HealthLake.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFHIRDatastore' smart constructor.
data CreateFHIRDatastore = CreateFHIRDatastore'
  { -- | Optional user provided token used for ensuring idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The user generated name for the Data Store.
    datastoreName :: Prelude.Maybe Prelude.Text,
    -- | Optional parameter to preload data upon creation of the Data Store.
    -- Currently, the only supported preloaded data is synthetic data generated
    -- from Synthea.
    preloadDataConfig :: Prelude.Maybe PreloadDataConfig,
    -- | The server-side encryption key configuration for a customer provided
    -- encryption key specified for creating a Data Store.
    sseConfiguration :: Prelude.Maybe SseConfiguration,
    -- | Resource tags that are applied to a Data Store when it is created.
    tags :: Prelude.Maybe [Tag],
    -- | The FHIR version of the Data Store. The only supported version is R4.
    datastoreTypeVersion :: FHIRVersion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFHIRDatastore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createFHIRDatastore_clientToken' - Optional user provided token used for ensuring idempotency.
--
-- 'datastoreName', 'createFHIRDatastore_datastoreName' - The user generated name for the Data Store.
--
-- 'preloadDataConfig', 'createFHIRDatastore_preloadDataConfig' - Optional parameter to preload data upon creation of the Data Store.
-- Currently, the only supported preloaded data is synthetic data generated
-- from Synthea.
--
-- 'sseConfiguration', 'createFHIRDatastore_sseConfiguration' - The server-side encryption key configuration for a customer provided
-- encryption key specified for creating a Data Store.
--
-- 'tags', 'createFHIRDatastore_tags' - Resource tags that are applied to a Data Store when it is created.
--
-- 'datastoreTypeVersion', 'createFHIRDatastore_datastoreTypeVersion' - The FHIR version of the Data Store. The only supported version is R4.
newCreateFHIRDatastore ::
  -- | 'datastoreTypeVersion'
  FHIRVersion ->
  CreateFHIRDatastore
newCreateFHIRDatastore pDatastoreTypeVersion_ =
  CreateFHIRDatastore'
    { clientToken = Prelude.Nothing,
      datastoreName = Prelude.Nothing,
      preloadDataConfig = Prelude.Nothing,
      sseConfiguration = Prelude.Nothing,
      tags = Prelude.Nothing,
      datastoreTypeVersion = pDatastoreTypeVersion_
    }

-- | Optional user provided token used for ensuring idempotency.
createFHIRDatastore_clientToken :: Lens.Lens' CreateFHIRDatastore (Prelude.Maybe Prelude.Text)
createFHIRDatastore_clientToken = Lens.lens (\CreateFHIRDatastore' {clientToken} -> clientToken) (\s@CreateFHIRDatastore' {} a -> s {clientToken = a} :: CreateFHIRDatastore)

-- | The user generated name for the Data Store.
createFHIRDatastore_datastoreName :: Lens.Lens' CreateFHIRDatastore (Prelude.Maybe Prelude.Text)
createFHIRDatastore_datastoreName = Lens.lens (\CreateFHIRDatastore' {datastoreName} -> datastoreName) (\s@CreateFHIRDatastore' {} a -> s {datastoreName = a} :: CreateFHIRDatastore)

-- | Optional parameter to preload data upon creation of the Data Store.
-- Currently, the only supported preloaded data is synthetic data generated
-- from Synthea.
createFHIRDatastore_preloadDataConfig :: Lens.Lens' CreateFHIRDatastore (Prelude.Maybe PreloadDataConfig)
createFHIRDatastore_preloadDataConfig = Lens.lens (\CreateFHIRDatastore' {preloadDataConfig} -> preloadDataConfig) (\s@CreateFHIRDatastore' {} a -> s {preloadDataConfig = a} :: CreateFHIRDatastore)

-- | The server-side encryption key configuration for a customer provided
-- encryption key specified for creating a Data Store.
createFHIRDatastore_sseConfiguration :: Lens.Lens' CreateFHIRDatastore (Prelude.Maybe SseConfiguration)
createFHIRDatastore_sseConfiguration = Lens.lens (\CreateFHIRDatastore' {sseConfiguration} -> sseConfiguration) (\s@CreateFHIRDatastore' {} a -> s {sseConfiguration = a} :: CreateFHIRDatastore)

-- | Resource tags that are applied to a Data Store when it is created.
createFHIRDatastore_tags :: Lens.Lens' CreateFHIRDatastore (Prelude.Maybe [Tag])
createFHIRDatastore_tags = Lens.lens (\CreateFHIRDatastore' {tags} -> tags) (\s@CreateFHIRDatastore' {} a -> s {tags = a} :: CreateFHIRDatastore) Prelude.. Lens.mapping Lens.coerced

-- | The FHIR version of the Data Store. The only supported version is R4.
createFHIRDatastore_datastoreTypeVersion :: Lens.Lens' CreateFHIRDatastore FHIRVersion
createFHIRDatastore_datastoreTypeVersion = Lens.lens (\CreateFHIRDatastore' {datastoreTypeVersion} -> datastoreTypeVersion) (\s@CreateFHIRDatastore' {} a -> s {datastoreTypeVersion = a} :: CreateFHIRDatastore)

instance Core.AWSRequest CreateFHIRDatastore where
  type
    AWSResponse CreateFHIRDatastore =
      CreateFHIRDatastoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFHIRDatastoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DatastoreId")
            Prelude.<*> (x Data..:> "DatastoreArn")
            Prelude.<*> (x Data..:> "DatastoreStatus")
            Prelude.<*> (x Data..:> "DatastoreEndpoint")
      )

instance Prelude.Hashable CreateFHIRDatastore where
  hashWithSalt _salt CreateFHIRDatastore' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` datastoreName
      `Prelude.hashWithSalt` preloadDataConfig
      `Prelude.hashWithSalt` sseConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` datastoreTypeVersion

instance Prelude.NFData CreateFHIRDatastore where
  rnf CreateFHIRDatastore' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf datastoreName
      `Prelude.seq` Prelude.rnf preloadDataConfig
      `Prelude.seq` Prelude.rnf sseConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf datastoreTypeVersion

instance Data.ToHeaders CreateFHIRDatastore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "HealthLake.CreateFHIRDatastore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFHIRDatastore where
  toJSON CreateFHIRDatastore' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("DatastoreName" Data..=) Prelude.<$> datastoreName,
            ("PreloadDataConfig" Data..=)
              Prelude.<$> preloadDataConfig,
            ("SseConfiguration" Data..=)
              Prelude.<$> sseConfiguration,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "DatastoreTypeVersion"
                  Data..= datastoreTypeVersion
              )
          ]
      )

instance Data.ToPath CreateFHIRDatastore where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateFHIRDatastore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFHIRDatastoreResponse' smart constructor.
data CreateFHIRDatastoreResponse = CreateFHIRDatastoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The AWS-generated Data Store id. This id is in the output from the
    -- initial Data Store creation call.
    datastoreId :: Prelude.Text,
    -- | The datastore ARN is generated during the creation of the Data Store and
    -- can be found in the output from the initial Data Store creation call.
    datastoreArn :: Prelude.Text,
    -- | The status of the FHIR Data Store. Possible statuses are ‘CREATING’,
    -- ‘ACTIVE’, ‘DELETING’, ‘DELETED’.
    datastoreStatus :: DatastoreStatus,
    -- | The AWS endpoint for the created Data Store. For preview, only US-east-1
    -- endpoints are supported.
    datastoreEndpoint :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFHIRDatastoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createFHIRDatastoreResponse_httpStatus' - The response's http status code.
--
-- 'datastoreId', 'createFHIRDatastoreResponse_datastoreId' - The AWS-generated Data Store id. This id is in the output from the
-- initial Data Store creation call.
--
-- 'datastoreArn', 'createFHIRDatastoreResponse_datastoreArn' - The datastore ARN is generated during the creation of the Data Store and
-- can be found in the output from the initial Data Store creation call.
--
-- 'datastoreStatus', 'createFHIRDatastoreResponse_datastoreStatus' - The status of the FHIR Data Store. Possible statuses are ‘CREATING’,
-- ‘ACTIVE’, ‘DELETING’, ‘DELETED’.
--
-- 'datastoreEndpoint', 'createFHIRDatastoreResponse_datastoreEndpoint' - The AWS endpoint for the created Data Store. For preview, only US-east-1
-- endpoints are supported.
newCreateFHIRDatastoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'datastoreId'
  Prelude.Text ->
  -- | 'datastoreArn'
  Prelude.Text ->
  -- | 'datastoreStatus'
  DatastoreStatus ->
  -- | 'datastoreEndpoint'
  Prelude.Text ->
  CreateFHIRDatastoreResponse
newCreateFHIRDatastoreResponse
  pHttpStatus_
  pDatastoreId_
  pDatastoreArn_
  pDatastoreStatus_
  pDatastoreEndpoint_ =
    CreateFHIRDatastoreResponse'
      { httpStatus =
          pHttpStatus_,
        datastoreId = pDatastoreId_,
        datastoreArn = pDatastoreArn_,
        datastoreStatus = pDatastoreStatus_,
        datastoreEndpoint = pDatastoreEndpoint_
      }

-- | The response's http status code.
createFHIRDatastoreResponse_httpStatus :: Lens.Lens' CreateFHIRDatastoreResponse Prelude.Int
createFHIRDatastoreResponse_httpStatus = Lens.lens (\CreateFHIRDatastoreResponse' {httpStatus} -> httpStatus) (\s@CreateFHIRDatastoreResponse' {} a -> s {httpStatus = a} :: CreateFHIRDatastoreResponse)

-- | The AWS-generated Data Store id. This id is in the output from the
-- initial Data Store creation call.
createFHIRDatastoreResponse_datastoreId :: Lens.Lens' CreateFHIRDatastoreResponse Prelude.Text
createFHIRDatastoreResponse_datastoreId = Lens.lens (\CreateFHIRDatastoreResponse' {datastoreId} -> datastoreId) (\s@CreateFHIRDatastoreResponse' {} a -> s {datastoreId = a} :: CreateFHIRDatastoreResponse)

-- | The datastore ARN is generated during the creation of the Data Store and
-- can be found in the output from the initial Data Store creation call.
createFHIRDatastoreResponse_datastoreArn :: Lens.Lens' CreateFHIRDatastoreResponse Prelude.Text
createFHIRDatastoreResponse_datastoreArn = Lens.lens (\CreateFHIRDatastoreResponse' {datastoreArn} -> datastoreArn) (\s@CreateFHIRDatastoreResponse' {} a -> s {datastoreArn = a} :: CreateFHIRDatastoreResponse)

-- | The status of the FHIR Data Store. Possible statuses are ‘CREATING’,
-- ‘ACTIVE’, ‘DELETING’, ‘DELETED’.
createFHIRDatastoreResponse_datastoreStatus :: Lens.Lens' CreateFHIRDatastoreResponse DatastoreStatus
createFHIRDatastoreResponse_datastoreStatus = Lens.lens (\CreateFHIRDatastoreResponse' {datastoreStatus} -> datastoreStatus) (\s@CreateFHIRDatastoreResponse' {} a -> s {datastoreStatus = a} :: CreateFHIRDatastoreResponse)

-- | The AWS endpoint for the created Data Store. For preview, only US-east-1
-- endpoints are supported.
createFHIRDatastoreResponse_datastoreEndpoint :: Lens.Lens' CreateFHIRDatastoreResponse Prelude.Text
createFHIRDatastoreResponse_datastoreEndpoint = Lens.lens (\CreateFHIRDatastoreResponse' {datastoreEndpoint} -> datastoreEndpoint) (\s@CreateFHIRDatastoreResponse' {} a -> s {datastoreEndpoint = a} :: CreateFHIRDatastoreResponse)

instance Prelude.NFData CreateFHIRDatastoreResponse where
  rnf CreateFHIRDatastoreResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf datastoreId
      `Prelude.seq` Prelude.rnf datastoreArn
      `Prelude.seq` Prelude.rnf datastoreStatus
      `Prelude.seq` Prelude.rnf datastoreEndpoint
