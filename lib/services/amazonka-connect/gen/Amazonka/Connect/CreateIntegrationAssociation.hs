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
-- Module      : Amazonka.Connect.CreateIntegrationAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Web Services resource association with an Amazon
-- Connect instance.
module Amazonka.Connect.CreateIntegrationAssociation
  ( -- * Creating a Request
    CreateIntegrationAssociation (..),
    newCreateIntegrationAssociation,

    -- * Request Lenses
    createIntegrationAssociation_tags,
    createIntegrationAssociation_sourceType,
    createIntegrationAssociation_sourceApplicationName,
    createIntegrationAssociation_sourceApplicationUrl,
    createIntegrationAssociation_instanceId,
    createIntegrationAssociation_integrationType,
    createIntegrationAssociation_integrationArn,

    -- * Destructuring the Response
    CreateIntegrationAssociationResponse (..),
    newCreateIntegrationAssociationResponse,

    -- * Response Lenses
    createIntegrationAssociationResponse_integrationAssociationArn,
    createIntegrationAssociationResponse_integrationAssociationId,
    createIntegrationAssociationResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateIntegrationAssociation' smart constructor.
data CreateIntegrationAssociation = CreateIntegrationAssociation'
  { -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The type of the data source. This field is only required for the EVENT
    -- integration type.
    sourceType :: Prelude.Maybe SourceType,
    -- | The name of the external application. This field is only required for
    -- the EVENT integration type.
    sourceApplicationName :: Prelude.Maybe Prelude.Text,
    -- | The URL for the external application. This field is only required for
    -- the EVENT integration type.
    sourceApplicationUrl :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The type of information to be ingested.
    integrationType :: IntegrationType,
    -- | The Amazon Resource Name (ARN) of the integration.
    --
    -- When integrating with Amazon Pinpoint, the Amazon Connect and Amazon
    -- Pinpoint instances must be in the same account.
    integrationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIntegrationAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createIntegrationAssociation_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'sourceType', 'createIntegrationAssociation_sourceType' - The type of the data source. This field is only required for the EVENT
-- integration type.
--
-- 'sourceApplicationName', 'createIntegrationAssociation_sourceApplicationName' - The name of the external application. This field is only required for
-- the EVENT integration type.
--
-- 'sourceApplicationUrl', 'createIntegrationAssociation_sourceApplicationUrl' - The URL for the external application. This field is only required for
-- the EVENT integration type.
--
-- 'instanceId', 'createIntegrationAssociation_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'integrationType', 'createIntegrationAssociation_integrationType' - The type of information to be ingested.
--
-- 'integrationArn', 'createIntegrationAssociation_integrationArn' - The Amazon Resource Name (ARN) of the integration.
--
-- When integrating with Amazon Pinpoint, the Amazon Connect and Amazon
-- Pinpoint instances must be in the same account.
newCreateIntegrationAssociation ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'integrationType'
  IntegrationType ->
  -- | 'integrationArn'
  Prelude.Text ->
  CreateIntegrationAssociation
newCreateIntegrationAssociation
  pInstanceId_
  pIntegrationType_
  pIntegrationArn_ =
    CreateIntegrationAssociation'
      { tags =
          Prelude.Nothing,
        sourceType = Prelude.Nothing,
        sourceApplicationName = Prelude.Nothing,
        sourceApplicationUrl = Prelude.Nothing,
        instanceId = pInstanceId_,
        integrationType = pIntegrationType_,
        integrationArn = pIntegrationArn_
      }

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
createIntegrationAssociation_tags :: Lens.Lens' CreateIntegrationAssociation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createIntegrationAssociation_tags = Lens.lens (\CreateIntegrationAssociation' {tags} -> tags) (\s@CreateIntegrationAssociation' {} a -> s {tags = a} :: CreateIntegrationAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The type of the data source. This field is only required for the EVENT
-- integration type.
createIntegrationAssociation_sourceType :: Lens.Lens' CreateIntegrationAssociation (Prelude.Maybe SourceType)
createIntegrationAssociation_sourceType = Lens.lens (\CreateIntegrationAssociation' {sourceType} -> sourceType) (\s@CreateIntegrationAssociation' {} a -> s {sourceType = a} :: CreateIntegrationAssociation)

-- | The name of the external application. This field is only required for
-- the EVENT integration type.
createIntegrationAssociation_sourceApplicationName :: Lens.Lens' CreateIntegrationAssociation (Prelude.Maybe Prelude.Text)
createIntegrationAssociation_sourceApplicationName = Lens.lens (\CreateIntegrationAssociation' {sourceApplicationName} -> sourceApplicationName) (\s@CreateIntegrationAssociation' {} a -> s {sourceApplicationName = a} :: CreateIntegrationAssociation)

-- | The URL for the external application. This field is only required for
-- the EVENT integration type.
createIntegrationAssociation_sourceApplicationUrl :: Lens.Lens' CreateIntegrationAssociation (Prelude.Maybe Prelude.Text)
createIntegrationAssociation_sourceApplicationUrl = Lens.lens (\CreateIntegrationAssociation' {sourceApplicationUrl} -> sourceApplicationUrl) (\s@CreateIntegrationAssociation' {} a -> s {sourceApplicationUrl = a} :: CreateIntegrationAssociation)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
createIntegrationAssociation_instanceId :: Lens.Lens' CreateIntegrationAssociation Prelude.Text
createIntegrationAssociation_instanceId = Lens.lens (\CreateIntegrationAssociation' {instanceId} -> instanceId) (\s@CreateIntegrationAssociation' {} a -> s {instanceId = a} :: CreateIntegrationAssociation)

-- | The type of information to be ingested.
createIntegrationAssociation_integrationType :: Lens.Lens' CreateIntegrationAssociation IntegrationType
createIntegrationAssociation_integrationType = Lens.lens (\CreateIntegrationAssociation' {integrationType} -> integrationType) (\s@CreateIntegrationAssociation' {} a -> s {integrationType = a} :: CreateIntegrationAssociation)

-- | The Amazon Resource Name (ARN) of the integration.
--
-- When integrating with Amazon Pinpoint, the Amazon Connect and Amazon
-- Pinpoint instances must be in the same account.
createIntegrationAssociation_integrationArn :: Lens.Lens' CreateIntegrationAssociation Prelude.Text
createIntegrationAssociation_integrationArn = Lens.lens (\CreateIntegrationAssociation' {integrationArn} -> integrationArn) (\s@CreateIntegrationAssociation' {} a -> s {integrationArn = a} :: CreateIntegrationAssociation)

instance Core.AWSRequest CreateIntegrationAssociation where
  type
    AWSResponse CreateIntegrationAssociation =
      CreateIntegrationAssociationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIntegrationAssociationResponse'
            Prelude.<$> (x Data..?> "IntegrationAssociationArn")
            Prelude.<*> (x Data..?> "IntegrationAssociationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateIntegrationAssociation
  where
  hashWithSalt _salt CreateIntegrationAssociation' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` sourceApplicationName
      `Prelude.hashWithSalt` sourceApplicationUrl
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` integrationType
      `Prelude.hashWithSalt` integrationArn

instance Prelude.NFData CreateIntegrationAssociation where
  rnf CreateIntegrationAssociation' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf sourceApplicationName
      `Prelude.seq` Prelude.rnf sourceApplicationUrl
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf integrationType
      `Prelude.seq` Prelude.rnf integrationArn

instance Data.ToHeaders CreateIntegrationAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateIntegrationAssociation where
  toJSON CreateIntegrationAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("SourceType" Data..=) Prelude.<$> sourceType,
            ("SourceApplicationName" Data..=)
              Prelude.<$> sourceApplicationName,
            ("SourceApplicationUrl" Data..=)
              Prelude.<$> sourceApplicationUrl,
            Prelude.Just
              ("IntegrationType" Data..= integrationType),
            Prelude.Just
              ("IntegrationArn" Data..= integrationArn)
          ]
      )

instance Data.ToPath CreateIntegrationAssociation where
  toPath CreateIntegrationAssociation' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/integration-associations"
      ]

instance Data.ToQuery CreateIntegrationAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateIntegrationAssociationResponse' smart constructor.
data CreateIntegrationAssociationResponse = CreateIntegrationAssociationResponse'
  { -- | The Amazon Resource Name (ARN) for the association.
    integrationAssociationArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the integration association.
    integrationAssociationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIntegrationAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'integrationAssociationArn', 'createIntegrationAssociationResponse_integrationAssociationArn' - The Amazon Resource Name (ARN) for the association.
--
-- 'integrationAssociationId', 'createIntegrationAssociationResponse_integrationAssociationId' - The identifier for the integration association.
--
-- 'httpStatus', 'createIntegrationAssociationResponse_httpStatus' - The response's http status code.
newCreateIntegrationAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateIntegrationAssociationResponse
newCreateIntegrationAssociationResponse pHttpStatus_ =
  CreateIntegrationAssociationResponse'
    { integrationAssociationArn =
        Prelude.Nothing,
      integrationAssociationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the association.
createIntegrationAssociationResponse_integrationAssociationArn :: Lens.Lens' CreateIntegrationAssociationResponse (Prelude.Maybe Prelude.Text)
createIntegrationAssociationResponse_integrationAssociationArn = Lens.lens (\CreateIntegrationAssociationResponse' {integrationAssociationArn} -> integrationAssociationArn) (\s@CreateIntegrationAssociationResponse' {} a -> s {integrationAssociationArn = a} :: CreateIntegrationAssociationResponse)

-- | The identifier for the integration association.
createIntegrationAssociationResponse_integrationAssociationId :: Lens.Lens' CreateIntegrationAssociationResponse (Prelude.Maybe Prelude.Text)
createIntegrationAssociationResponse_integrationAssociationId = Lens.lens (\CreateIntegrationAssociationResponse' {integrationAssociationId} -> integrationAssociationId) (\s@CreateIntegrationAssociationResponse' {} a -> s {integrationAssociationId = a} :: CreateIntegrationAssociationResponse)

-- | The response's http status code.
createIntegrationAssociationResponse_httpStatus :: Lens.Lens' CreateIntegrationAssociationResponse Prelude.Int
createIntegrationAssociationResponse_httpStatus = Lens.lens (\CreateIntegrationAssociationResponse' {httpStatus} -> httpStatus) (\s@CreateIntegrationAssociationResponse' {} a -> s {httpStatus = a} :: CreateIntegrationAssociationResponse)

instance
  Prelude.NFData
    CreateIntegrationAssociationResponse
  where
  rnf CreateIntegrationAssociationResponse' {..} =
    Prelude.rnf integrationAssociationArn
      `Prelude.seq` Prelude.rnf integrationAssociationId
      `Prelude.seq` Prelude.rnf httpStatus
