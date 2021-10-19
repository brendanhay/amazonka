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
-- Module      : Network.AWS.Connect.CreateIntegrationAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS resource association with an Amazon Connect instance.
module Network.AWS.Connect.CreateIntegrationAssociation
  ( -- * Creating a Request
    CreateIntegrationAssociation (..),
    newCreateIntegrationAssociation,

    -- * Request Lenses
    createIntegrationAssociation_sourceType,
    createIntegrationAssociation_sourceApplicationUrl,
    createIntegrationAssociation_sourceApplicationName,
    createIntegrationAssociation_tags,
    createIntegrationAssociation_instanceId,
    createIntegrationAssociation_integrationType,
    createIntegrationAssociation_integrationArn,

    -- * Destructuring the Response
    CreateIntegrationAssociationResponse (..),
    newCreateIntegrationAssociationResponse,

    -- * Response Lenses
    createIntegrationAssociationResponse_integrationAssociationId,
    createIntegrationAssociationResponse_integrationAssociationArn,
    createIntegrationAssociationResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateIntegrationAssociation' smart constructor.
data CreateIntegrationAssociation = CreateIntegrationAssociation'
  { -- | The type of the data source. This field is only required for the EVENT
    -- integration type.
    sourceType :: Prelude.Maybe SourceType,
    -- | The URL for the external application. This field is only required for
    -- the EVENT integration type.
    sourceApplicationUrl :: Prelude.Maybe Prelude.Text,
    -- | The name of the external application. This field is only required for
    -- the EVENT integration type.
    sourceApplicationName :: Prelude.Maybe Prelude.Text,
    -- | One or more tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The type of information to be ingested.
    integrationType :: IntegrationType,
    -- | The Amazon Resource Name (ARN) of the integration.
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
-- 'sourceType', 'createIntegrationAssociation_sourceType' - The type of the data source. This field is only required for the EVENT
-- integration type.
--
-- 'sourceApplicationUrl', 'createIntegrationAssociation_sourceApplicationUrl' - The URL for the external application. This field is only required for
-- the EVENT integration type.
--
-- 'sourceApplicationName', 'createIntegrationAssociation_sourceApplicationName' - The name of the external application. This field is only required for
-- the EVENT integration type.
--
-- 'tags', 'createIntegrationAssociation_tags' - One or more tags.
--
-- 'instanceId', 'createIntegrationAssociation_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'integrationType', 'createIntegrationAssociation_integrationType' - The type of information to be ingested.
--
-- 'integrationArn', 'createIntegrationAssociation_integrationArn' - The Amazon Resource Name (ARN) of the integration.
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
      { sourceType =
          Prelude.Nothing,
        sourceApplicationUrl = Prelude.Nothing,
        sourceApplicationName = Prelude.Nothing,
        tags = Prelude.Nothing,
        instanceId = pInstanceId_,
        integrationType = pIntegrationType_,
        integrationArn = pIntegrationArn_
      }

-- | The type of the data source. This field is only required for the EVENT
-- integration type.
createIntegrationAssociation_sourceType :: Lens.Lens' CreateIntegrationAssociation (Prelude.Maybe SourceType)
createIntegrationAssociation_sourceType = Lens.lens (\CreateIntegrationAssociation' {sourceType} -> sourceType) (\s@CreateIntegrationAssociation' {} a -> s {sourceType = a} :: CreateIntegrationAssociation)

-- | The URL for the external application. This field is only required for
-- the EVENT integration type.
createIntegrationAssociation_sourceApplicationUrl :: Lens.Lens' CreateIntegrationAssociation (Prelude.Maybe Prelude.Text)
createIntegrationAssociation_sourceApplicationUrl = Lens.lens (\CreateIntegrationAssociation' {sourceApplicationUrl} -> sourceApplicationUrl) (\s@CreateIntegrationAssociation' {} a -> s {sourceApplicationUrl = a} :: CreateIntegrationAssociation)

-- | The name of the external application. This field is only required for
-- the EVENT integration type.
createIntegrationAssociation_sourceApplicationName :: Lens.Lens' CreateIntegrationAssociation (Prelude.Maybe Prelude.Text)
createIntegrationAssociation_sourceApplicationName = Lens.lens (\CreateIntegrationAssociation' {sourceApplicationName} -> sourceApplicationName) (\s@CreateIntegrationAssociation' {} a -> s {sourceApplicationName = a} :: CreateIntegrationAssociation)

-- | One or more tags.
createIntegrationAssociation_tags :: Lens.Lens' CreateIntegrationAssociation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createIntegrationAssociation_tags = Lens.lens (\CreateIntegrationAssociation' {tags} -> tags) (\s@CreateIntegrationAssociation' {} a -> s {tags = a} :: CreateIntegrationAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
createIntegrationAssociation_instanceId :: Lens.Lens' CreateIntegrationAssociation Prelude.Text
createIntegrationAssociation_instanceId = Lens.lens (\CreateIntegrationAssociation' {instanceId} -> instanceId) (\s@CreateIntegrationAssociation' {} a -> s {instanceId = a} :: CreateIntegrationAssociation)

-- | The type of information to be ingested.
createIntegrationAssociation_integrationType :: Lens.Lens' CreateIntegrationAssociation IntegrationType
createIntegrationAssociation_integrationType = Lens.lens (\CreateIntegrationAssociation' {integrationType} -> integrationType) (\s@CreateIntegrationAssociation' {} a -> s {integrationType = a} :: CreateIntegrationAssociation)

-- | The Amazon Resource Name (ARN) of the integration.
createIntegrationAssociation_integrationArn :: Lens.Lens' CreateIntegrationAssociation Prelude.Text
createIntegrationAssociation_integrationArn = Lens.lens (\CreateIntegrationAssociation' {integrationArn} -> integrationArn) (\s@CreateIntegrationAssociation' {} a -> s {integrationArn = a} :: CreateIntegrationAssociation)

instance Core.AWSRequest CreateIntegrationAssociation where
  type
    AWSResponse CreateIntegrationAssociation =
      CreateIntegrationAssociationResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIntegrationAssociationResponse'
            Prelude.<$> (x Core..?> "IntegrationAssociationId")
            Prelude.<*> (x Core..?> "IntegrationAssociationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateIntegrationAssociation

instance Prelude.NFData CreateIntegrationAssociation

instance Core.ToHeaders CreateIntegrationAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateIntegrationAssociation where
  toJSON CreateIntegrationAssociation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SourceType" Core..=) Prelude.<$> sourceType,
            ("SourceApplicationUrl" Core..=)
              Prelude.<$> sourceApplicationUrl,
            ("SourceApplicationName" Core..=)
              Prelude.<$> sourceApplicationName,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("IntegrationType" Core..= integrationType),
            Prelude.Just
              ("IntegrationArn" Core..= integrationArn)
          ]
      )

instance Core.ToPath CreateIntegrationAssociation where
  toPath CreateIntegrationAssociation' {..} =
    Prelude.mconcat
      [ "/instance/",
        Core.toBS instanceId,
        "/integration-associations"
      ]

instance Core.ToQuery CreateIntegrationAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateIntegrationAssociationResponse' smart constructor.
data CreateIntegrationAssociationResponse = CreateIntegrationAssociationResponse'
  { -- | The identifier for the integration association.
    integrationAssociationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the association.
    integrationAssociationArn :: Prelude.Maybe Prelude.Text,
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
-- 'integrationAssociationId', 'createIntegrationAssociationResponse_integrationAssociationId' - The identifier for the integration association.
--
-- 'integrationAssociationArn', 'createIntegrationAssociationResponse_integrationAssociationArn' - The Amazon Resource Name (ARN) for the association.
--
-- 'httpStatus', 'createIntegrationAssociationResponse_httpStatus' - The response's http status code.
newCreateIntegrationAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateIntegrationAssociationResponse
newCreateIntegrationAssociationResponse pHttpStatus_ =
  CreateIntegrationAssociationResponse'
    { integrationAssociationId =
        Prelude.Nothing,
      integrationAssociationArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for the integration association.
createIntegrationAssociationResponse_integrationAssociationId :: Lens.Lens' CreateIntegrationAssociationResponse (Prelude.Maybe Prelude.Text)
createIntegrationAssociationResponse_integrationAssociationId = Lens.lens (\CreateIntegrationAssociationResponse' {integrationAssociationId} -> integrationAssociationId) (\s@CreateIntegrationAssociationResponse' {} a -> s {integrationAssociationId = a} :: CreateIntegrationAssociationResponse)

-- | The Amazon Resource Name (ARN) for the association.
createIntegrationAssociationResponse_integrationAssociationArn :: Lens.Lens' CreateIntegrationAssociationResponse (Prelude.Maybe Prelude.Text)
createIntegrationAssociationResponse_integrationAssociationArn = Lens.lens (\CreateIntegrationAssociationResponse' {integrationAssociationArn} -> integrationAssociationArn) (\s@CreateIntegrationAssociationResponse' {} a -> s {integrationAssociationArn = a} :: CreateIntegrationAssociationResponse)

-- | The response's http status code.
createIntegrationAssociationResponse_httpStatus :: Lens.Lens' CreateIntegrationAssociationResponse Prelude.Int
createIntegrationAssociationResponse_httpStatus = Lens.lens (\CreateIntegrationAssociationResponse' {httpStatus} -> httpStatus) (\s@CreateIntegrationAssociationResponse' {} a -> s {httpStatus = a} :: CreateIntegrationAssociationResponse)

instance
  Prelude.NFData
    CreateIntegrationAssociationResponse
