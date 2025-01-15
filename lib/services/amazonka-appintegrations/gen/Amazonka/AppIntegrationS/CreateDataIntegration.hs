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
-- Module      : Amazonka.AppIntegrationS.CreateDataIntegration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and persists a DataIntegration resource.
--
-- You cannot create a DataIntegration association for a DataIntegration
-- that has been previously associated. Use a different DataIntegration, or
-- recreate the DataIntegration using the @CreateDataIntegration@ API.
module Amazonka.AppIntegrationS.CreateDataIntegration
  ( -- * Creating a Request
    CreateDataIntegration (..),
    newCreateDataIntegration,

    -- * Request Lenses
    createDataIntegration_clientToken,
    createDataIntegration_description,
    createDataIntegration_kmsKey,
    createDataIntegration_scheduleConfig,
    createDataIntegration_sourceURI,
    createDataIntegration_tags,
    createDataIntegration_name,

    -- * Destructuring the Response
    CreateDataIntegrationResponse (..),
    newCreateDataIntegrationResponse,

    -- * Response Lenses
    createDataIntegrationResponse_arn,
    createDataIntegrationResponse_clientToken,
    createDataIntegrationResponse_description,
    createDataIntegrationResponse_id,
    createDataIntegrationResponse_kmsKey,
    createDataIntegrationResponse_name,
    createDataIntegrationResponse_scheduleConfiguration,
    createDataIntegrationResponse_sourceURI,
    createDataIntegrationResponse_tags,
    createDataIntegrationResponse_httpStatus,
  )
where

import Amazonka.AppIntegrationS.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDataIntegration' smart constructor.
data CreateDataIntegration = CreateDataIntegration'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description of the DataIntegration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The KMS key for the DataIntegration.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The name of the data and how often it should be pulled from the source.
    scheduleConfig :: Prelude.Maybe ScheduleConfiguration,
    -- | The URI of the data source.
    sourceURI :: Prelude.Maybe Prelude.Text,
    -- | One or more tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the DataIntegration.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createDataIntegration_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'description', 'createDataIntegration_description' - A description of the DataIntegration.
--
-- 'kmsKey', 'createDataIntegration_kmsKey' - The KMS key for the DataIntegration.
--
-- 'scheduleConfig', 'createDataIntegration_scheduleConfig' - The name of the data and how often it should be pulled from the source.
--
-- 'sourceURI', 'createDataIntegration_sourceURI' - The URI of the data source.
--
-- 'tags', 'createDataIntegration_tags' - One or more tags.
--
-- 'name', 'createDataIntegration_name' - The name of the DataIntegration.
newCreateDataIntegration ::
  -- | 'name'
  Prelude.Text ->
  CreateDataIntegration
newCreateDataIntegration pName_ =
  CreateDataIntegration'
    { clientToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      scheduleConfig = Prelude.Nothing,
      sourceURI = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createDataIntegration_clientToken :: Lens.Lens' CreateDataIntegration (Prelude.Maybe Prelude.Text)
createDataIntegration_clientToken = Lens.lens (\CreateDataIntegration' {clientToken} -> clientToken) (\s@CreateDataIntegration' {} a -> s {clientToken = a} :: CreateDataIntegration)

-- | A description of the DataIntegration.
createDataIntegration_description :: Lens.Lens' CreateDataIntegration (Prelude.Maybe Prelude.Text)
createDataIntegration_description = Lens.lens (\CreateDataIntegration' {description} -> description) (\s@CreateDataIntegration' {} a -> s {description = a} :: CreateDataIntegration)

-- | The KMS key for the DataIntegration.
createDataIntegration_kmsKey :: Lens.Lens' CreateDataIntegration (Prelude.Maybe Prelude.Text)
createDataIntegration_kmsKey = Lens.lens (\CreateDataIntegration' {kmsKey} -> kmsKey) (\s@CreateDataIntegration' {} a -> s {kmsKey = a} :: CreateDataIntegration)

-- | The name of the data and how often it should be pulled from the source.
createDataIntegration_scheduleConfig :: Lens.Lens' CreateDataIntegration (Prelude.Maybe ScheduleConfiguration)
createDataIntegration_scheduleConfig = Lens.lens (\CreateDataIntegration' {scheduleConfig} -> scheduleConfig) (\s@CreateDataIntegration' {} a -> s {scheduleConfig = a} :: CreateDataIntegration)

-- | The URI of the data source.
createDataIntegration_sourceURI :: Lens.Lens' CreateDataIntegration (Prelude.Maybe Prelude.Text)
createDataIntegration_sourceURI = Lens.lens (\CreateDataIntegration' {sourceURI} -> sourceURI) (\s@CreateDataIntegration' {} a -> s {sourceURI = a} :: CreateDataIntegration)

-- | One or more tags.
createDataIntegration_tags :: Lens.Lens' CreateDataIntegration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDataIntegration_tags = Lens.lens (\CreateDataIntegration' {tags} -> tags) (\s@CreateDataIntegration' {} a -> s {tags = a} :: CreateDataIntegration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the DataIntegration.
createDataIntegration_name :: Lens.Lens' CreateDataIntegration Prelude.Text
createDataIntegration_name = Lens.lens (\CreateDataIntegration' {name} -> name) (\s@CreateDataIntegration' {} a -> s {name = a} :: CreateDataIntegration)

instance Core.AWSRequest CreateDataIntegration where
  type
    AWSResponse CreateDataIntegration =
      CreateDataIntegrationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataIntegrationResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "ClientToken")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "KmsKey")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "ScheduleConfiguration")
            Prelude.<*> (x Data..?> "SourceURI")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataIntegration where
  hashWithSalt _salt CreateDataIntegration' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` scheduleConfig
      `Prelude.hashWithSalt` sourceURI
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateDataIntegration where
  rnf CreateDataIntegration' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf kmsKey `Prelude.seq`
          Prelude.rnf scheduleConfig `Prelude.seq`
            Prelude.rnf sourceURI `Prelude.seq`
              Prelude.rnf tags `Prelude.seq`
                Prelude.rnf name

instance Data.ToHeaders CreateDataIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDataIntegration where
  toJSON CreateDataIntegration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("KmsKey" Data..=) Prelude.<$> kmsKey,
            ("ScheduleConfig" Data..=)
              Prelude.<$> scheduleConfig,
            ("SourceURI" Data..=) Prelude.<$> sourceURI,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateDataIntegration where
  toPath = Prelude.const "/dataIntegrations"

instance Data.ToQuery CreateDataIntegration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDataIntegrationResponse' smart constructor.
data CreateDataIntegrationResponse = CreateDataIntegrationResponse'
  { -- | The Amazon Resource Name (ARN)
    arn :: Prelude.Maybe Prelude.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description of the DataIntegration.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The KMS key for the DataIntegration.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The name of the DataIntegration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the data and how often it should be pulled from the source.
    scheduleConfiguration :: Prelude.Maybe ScheduleConfiguration,
    -- | The URI of the data source.
    sourceURI :: Prelude.Maybe Prelude.Text,
    -- | One or more tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createDataIntegrationResponse_arn' - The Amazon Resource Name (ARN)
--
-- 'clientToken', 'createDataIntegrationResponse_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'description', 'createDataIntegrationResponse_description' - A description of the DataIntegration.
--
-- 'id', 'createDataIntegrationResponse_id' - A unique identifier.
--
-- 'kmsKey', 'createDataIntegrationResponse_kmsKey' - The KMS key for the DataIntegration.
--
-- 'name', 'createDataIntegrationResponse_name' - The name of the DataIntegration.
--
-- 'scheduleConfiguration', 'createDataIntegrationResponse_scheduleConfiguration' - The name of the data and how often it should be pulled from the source.
--
-- 'sourceURI', 'createDataIntegrationResponse_sourceURI' - The URI of the data source.
--
-- 'tags', 'createDataIntegrationResponse_tags' - One or more tags.
--
-- 'httpStatus', 'createDataIntegrationResponse_httpStatus' - The response's http status code.
newCreateDataIntegrationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDataIntegrationResponse
newCreateDataIntegrationResponse pHttpStatus_ =
  CreateDataIntegrationResponse'
    { arn =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      name = Prelude.Nothing,
      scheduleConfiguration = Prelude.Nothing,
      sourceURI = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN)
createDataIntegrationResponse_arn :: Lens.Lens' CreateDataIntegrationResponse (Prelude.Maybe Prelude.Text)
createDataIntegrationResponse_arn = Lens.lens (\CreateDataIntegrationResponse' {arn} -> arn) (\s@CreateDataIntegrationResponse' {} a -> s {arn = a} :: CreateDataIntegrationResponse)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createDataIntegrationResponse_clientToken :: Lens.Lens' CreateDataIntegrationResponse (Prelude.Maybe Prelude.Text)
createDataIntegrationResponse_clientToken = Lens.lens (\CreateDataIntegrationResponse' {clientToken} -> clientToken) (\s@CreateDataIntegrationResponse' {} a -> s {clientToken = a} :: CreateDataIntegrationResponse)

-- | A description of the DataIntegration.
createDataIntegrationResponse_description :: Lens.Lens' CreateDataIntegrationResponse (Prelude.Maybe Prelude.Text)
createDataIntegrationResponse_description = Lens.lens (\CreateDataIntegrationResponse' {description} -> description) (\s@CreateDataIntegrationResponse' {} a -> s {description = a} :: CreateDataIntegrationResponse)

-- | A unique identifier.
createDataIntegrationResponse_id :: Lens.Lens' CreateDataIntegrationResponse (Prelude.Maybe Prelude.Text)
createDataIntegrationResponse_id = Lens.lens (\CreateDataIntegrationResponse' {id} -> id) (\s@CreateDataIntegrationResponse' {} a -> s {id = a} :: CreateDataIntegrationResponse)

-- | The KMS key for the DataIntegration.
createDataIntegrationResponse_kmsKey :: Lens.Lens' CreateDataIntegrationResponse (Prelude.Maybe Prelude.Text)
createDataIntegrationResponse_kmsKey = Lens.lens (\CreateDataIntegrationResponse' {kmsKey} -> kmsKey) (\s@CreateDataIntegrationResponse' {} a -> s {kmsKey = a} :: CreateDataIntegrationResponse)

-- | The name of the DataIntegration.
createDataIntegrationResponse_name :: Lens.Lens' CreateDataIntegrationResponse (Prelude.Maybe Prelude.Text)
createDataIntegrationResponse_name = Lens.lens (\CreateDataIntegrationResponse' {name} -> name) (\s@CreateDataIntegrationResponse' {} a -> s {name = a} :: CreateDataIntegrationResponse)

-- | The name of the data and how often it should be pulled from the source.
createDataIntegrationResponse_scheduleConfiguration :: Lens.Lens' CreateDataIntegrationResponse (Prelude.Maybe ScheduleConfiguration)
createDataIntegrationResponse_scheduleConfiguration = Lens.lens (\CreateDataIntegrationResponse' {scheduleConfiguration} -> scheduleConfiguration) (\s@CreateDataIntegrationResponse' {} a -> s {scheduleConfiguration = a} :: CreateDataIntegrationResponse)

-- | The URI of the data source.
createDataIntegrationResponse_sourceURI :: Lens.Lens' CreateDataIntegrationResponse (Prelude.Maybe Prelude.Text)
createDataIntegrationResponse_sourceURI = Lens.lens (\CreateDataIntegrationResponse' {sourceURI} -> sourceURI) (\s@CreateDataIntegrationResponse' {} a -> s {sourceURI = a} :: CreateDataIntegrationResponse)

-- | One or more tags.
createDataIntegrationResponse_tags :: Lens.Lens' CreateDataIntegrationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDataIntegrationResponse_tags = Lens.lens (\CreateDataIntegrationResponse' {tags} -> tags) (\s@CreateDataIntegrationResponse' {} a -> s {tags = a} :: CreateDataIntegrationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createDataIntegrationResponse_httpStatus :: Lens.Lens' CreateDataIntegrationResponse Prelude.Int
createDataIntegrationResponse_httpStatus = Lens.lens (\CreateDataIntegrationResponse' {httpStatus} -> httpStatus) (\s@CreateDataIntegrationResponse' {} a -> s {httpStatus = a} :: CreateDataIntegrationResponse)

instance Prelude.NFData CreateDataIntegrationResponse where
  rnf CreateDataIntegrationResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf clientToken `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf kmsKey `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf scheduleConfiguration `Prelude.seq`
                  Prelude.rnf sourceURI `Prelude.seq`
                    Prelude.rnf tags `Prelude.seq`
                      Prelude.rnf httpStatus
