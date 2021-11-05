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
-- Module      : Network.AWS.AppIntegrationS.CreateDataIntegration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and persists a DataIntegration resource.
--
-- You cannot create a DataIntegration association for a DataIntegration
-- that has been previously associated. Use a different DataIntegration, or
-- recreate the DataIntegration using the @CreateDataIntegration@ API.
module Network.AWS.AppIntegrationS.CreateDataIntegration
  ( -- * Creating a Request
    CreateDataIntegration (..),
    newCreateDataIntegration,

    -- * Request Lenses
    createDataIntegration_scheduleConfig,
    createDataIntegration_clientToken,
    createDataIntegration_kmsKey,
    createDataIntegration_sourceURI,
    createDataIntegration_description,
    createDataIntegration_tags,
    createDataIntegration_name,

    -- * Destructuring the Response
    CreateDataIntegrationResponse (..),
    newCreateDataIntegrationResponse,

    -- * Response Lenses
    createDataIntegrationResponse_clientToken,
    createDataIntegrationResponse_kmsKey,
    createDataIntegrationResponse_arn,
    createDataIntegrationResponse_scheduleConfiguration,
    createDataIntegrationResponse_name,
    createDataIntegrationResponse_id,
    createDataIntegrationResponse_sourceURI,
    createDataIntegrationResponse_description,
    createDataIntegrationResponse_tags,
    createDataIntegrationResponse_httpStatus,
  )
where

import Network.AWS.AppIntegrationS.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDataIntegration' smart constructor.
data CreateDataIntegration = CreateDataIntegration'
  { -- | The name of the data and how often it should be pulled from the source.
    scheduleConfig :: Prelude.Maybe ScheduleConfiguration,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The KMS key for the DataIntegration.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The URI of the data source.
    sourceURI :: Prelude.Maybe Prelude.Text,
    -- | A description of the DataIntegration.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'scheduleConfig', 'createDataIntegration_scheduleConfig' - The name of the data and how often it should be pulled from the source.
--
-- 'clientToken', 'createDataIntegration_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'kmsKey', 'createDataIntegration_kmsKey' - The KMS key for the DataIntegration.
--
-- 'sourceURI', 'createDataIntegration_sourceURI' - The URI of the data source.
--
-- 'description', 'createDataIntegration_description' - A description of the DataIntegration.
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
    { scheduleConfig =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      sourceURI = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | The name of the data and how often it should be pulled from the source.
createDataIntegration_scheduleConfig :: Lens.Lens' CreateDataIntegration (Prelude.Maybe ScheduleConfiguration)
createDataIntegration_scheduleConfig = Lens.lens (\CreateDataIntegration' {scheduleConfig} -> scheduleConfig) (\s@CreateDataIntegration' {} a -> s {scheduleConfig = a} :: CreateDataIntegration)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createDataIntegration_clientToken :: Lens.Lens' CreateDataIntegration (Prelude.Maybe Prelude.Text)
createDataIntegration_clientToken = Lens.lens (\CreateDataIntegration' {clientToken} -> clientToken) (\s@CreateDataIntegration' {} a -> s {clientToken = a} :: CreateDataIntegration)

-- | The KMS key for the DataIntegration.
createDataIntegration_kmsKey :: Lens.Lens' CreateDataIntegration (Prelude.Maybe Prelude.Text)
createDataIntegration_kmsKey = Lens.lens (\CreateDataIntegration' {kmsKey} -> kmsKey) (\s@CreateDataIntegration' {} a -> s {kmsKey = a} :: CreateDataIntegration)

-- | The URI of the data source.
createDataIntegration_sourceURI :: Lens.Lens' CreateDataIntegration (Prelude.Maybe Prelude.Text)
createDataIntegration_sourceURI = Lens.lens (\CreateDataIntegration' {sourceURI} -> sourceURI) (\s@CreateDataIntegration' {} a -> s {sourceURI = a} :: CreateDataIntegration)

-- | A description of the DataIntegration.
createDataIntegration_description :: Lens.Lens' CreateDataIntegration (Prelude.Maybe Prelude.Text)
createDataIntegration_description = Lens.lens (\CreateDataIntegration' {description} -> description) (\s@CreateDataIntegration' {} a -> s {description = a} :: CreateDataIntegration)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataIntegrationResponse'
            Prelude.<$> (x Core..?> "ClientToken")
            Prelude.<*> (x Core..?> "KmsKey")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "ScheduleConfiguration")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "SourceURI")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataIntegration

instance Prelude.NFData CreateDataIntegration

instance Core.ToHeaders CreateDataIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDataIntegration where
  toJSON CreateDataIntegration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ScheduleConfig" Core..=)
              Prelude.<$> scheduleConfig,
            ("ClientToken" Core..=) Prelude.<$> clientToken,
            ("KmsKey" Core..=) Prelude.<$> kmsKey,
            ("SourceURI" Core..=) Prelude.<$> sourceURI,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateDataIntegration where
  toPath = Prelude.const "/dataIntegrations"

instance Core.ToQuery CreateDataIntegration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDataIntegrationResponse' smart constructor.
data CreateDataIntegrationResponse = CreateDataIntegrationResponse'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The KMS key for the DataIntegration.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN)
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the data and how often it should be pulled from the source.
    scheduleConfiguration :: Prelude.Maybe ScheduleConfiguration,
    -- | The name of the DataIntegration.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The URI of the data source.
    sourceURI :: Prelude.Maybe Prelude.Text,
    -- | A description of the DataIntegration.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'clientToken', 'createDataIntegrationResponse_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'kmsKey', 'createDataIntegrationResponse_kmsKey' - The KMS key for the DataIntegration.
--
-- 'arn', 'createDataIntegrationResponse_arn' - The Amazon Resource Name (ARN)
--
-- 'scheduleConfiguration', 'createDataIntegrationResponse_scheduleConfiguration' - The name of the data and how often it should be pulled from the source.
--
-- 'name', 'createDataIntegrationResponse_name' - The name of the DataIntegration.
--
-- 'id', 'createDataIntegrationResponse_id' - A unique identifier.
--
-- 'sourceURI', 'createDataIntegrationResponse_sourceURI' - The URI of the data source.
--
-- 'description', 'createDataIntegrationResponse_description' - A description of the DataIntegration.
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
    { clientToken =
        Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      arn = Prelude.Nothing,
      scheduleConfiguration = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      sourceURI = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createDataIntegrationResponse_clientToken :: Lens.Lens' CreateDataIntegrationResponse (Prelude.Maybe Prelude.Text)
createDataIntegrationResponse_clientToken = Lens.lens (\CreateDataIntegrationResponse' {clientToken} -> clientToken) (\s@CreateDataIntegrationResponse' {} a -> s {clientToken = a} :: CreateDataIntegrationResponse)

-- | The KMS key for the DataIntegration.
createDataIntegrationResponse_kmsKey :: Lens.Lens' CreateDataIntegrationResponse (Prelude.Maybe Prelude.Text)
createDataIntegrationResponse_kmsKey = Lens.lens (\CreateDataIntegrationResponse' {kmsKey} -> kmsKey) (\s@CreateDataIntegrationResponse' {} a -> s {kmsKey = a} :: CreateDataIntegrationResponse)

-- | The Amazon Resource Name (ARN)
createDataIntegrationResponse_arn :: Lens.Lens' CreateDataIntegrationResponse (Prelude.Maybe Prelude.Text)
createDataIntegrationResponse_arn = Lens.lens (\CreateDataIntegrationResponse' {arn} -> arn) (\s@CreateDataIntegrationResponse' {} a -> s {arn = a} :: CreateDataIntegrationResponse)

-- | The name of the data and how often it should be pulled from the source.
createDataIntegrationResponse_scheduleConfiguration :: Lens.Lens' CreateDataIntegrationResponse (Prelude.Maybe ScheduleConfiguration)
createDataIntegrationResponse_scheduleConfiguration = Lens.lens (\CreateDataIntegrationResponse' {scheduleConfiguration} -> scheduleConfiguration) (\s@CreateDataIntegrationResponse' {} a -> s {scheduleConfiguration = a} :: CreateDataIntegrationResponse)

-- | The name of the DataIntegration.
createDataIntegrationResponse_name :: Lens.Lens' CreateDataIntegrationResponse (Prelude.Maybe Prelude.Text)
createDataIntegrationResponse_name = Lens.lens (\CreateDataIntegrationResponse' {name} -> name) (\s@CreateDataIntegrationResponse' {} a -> s {name = a} :: CreateDataIntegrationResponse)

-- | A unique identifier.
createDataIntegrationResponse_id :: Lens.Lens' CreateDataIntegrationResponse (Prelude.Maybe Prelude.Text)
createDataIntegrationResponse_id = Lens.lens (\CreateDataIntegrationResponse' {id} -> id) (\s@CreateDataIntegrationResponse' {} a -> s {id = a} :: CreateDataIntegrationResponse)

-- | The URI of the data source.
createDataIntegrationResponse_sourceURI :: Lens.Lens' CreateDataIntegrationResponse (Prelude.Maybe Prelude.Text)
createDataIntegrationResponse_sourceURI = Lens.lens (\CreateDataIntegrationResponse' {sourceURI} -> sourceURI) (\s@CreateDataIntegrationResponse' {} a -> s {sourceURI = a} :: CreateDataIntegrationResponse)

-- | A description of the DataIntegration.
createDataIntegrationResponse_description :: Lens.Lens' CreateDataIntegrationResponse (Prelude.Maybe Prelude.Text)
createDataIntegrationResponse_description = Lens.lens (\CreateDataIntegrationResponse' {description} -> description) (\s@CreateDataIntegrationResponse' {} a -> s {description = a} :: CreateDataIntegrationResponse)

-- | One or more tags.
createDataIntegrationResponse_tags :: Lens.Lens' CreateDataIntegrationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDataIntegrationResponse_tags = Lens.lens (\CreateDataIntegrationResponse' {tags} -> tags) (\s@CreateDataIntegrationResponse' {} a -> s {tags = a} :: CreateDataIntegrationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createDataIntegrationResponse_httpStatus :: Lens.Lens' CreateDataIntegrationResponse Prelude.Int
createDataIntegrationResponse_httpStatus = Lens.lens (\CreateDataIntegrationResponse' {httpStatus} -> httpStatus) (\s@CreateDataIntegrationResponse' {} a -> s {httpStatus = a} :: CreateDataIntegrationResponse)

instance Prelude.NFData CreateDataIntegrationResponse
