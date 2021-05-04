{-# LANGUAGE DeriveDataTypeable #-}
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
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Create an AppIntegration association with an Amazon Connect instance.
module Network.AWS.Connect.CreateIntegrationAssociation
  ( -- * Creating a Request
    CreateIntegrationAssociation (..),
    newCreateIntegrationAssociation,

    -- * Request Lenses
    createIntegrationAssociation_instanceId,
    createIntegrationAssociation_integrationType,
    createIntegrationAssociation_integrationArn,
    createIntegrationAssociation_sourceApplicationUrl,
    createIntegrationAssociation_sourceApplicationName,
    createIntegrationAssociation_sourceType,

    -- * Destructuring the Response
    CreateIntegrationAssociationResponse (..),
    newCreateIntegrationAssociationResponse,

    -- * Response Lenses
    createIntegrationAssociationResponse_integrationAssociationArn,
    createIntegrationAssociationResponse_integrationAssociationId,
    createIntegrationAssociationResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateIntegrationAssociation' smart constructor.
data CreateIntegrationAssociation = CreateIntegrationAssociation'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The type of information to be ingested.
    integrationType :: IntegrationType,
    -- | The Amazon Resource Name (ARN) of the integration.
    integrationArn :: Prelude.Text,
    -- | The URL for the external application.
    sourceApplicationUrl :: Prelude.Text,
    -- | The name of the external application.
    sourceApplicationName :: Prelude.Text,
    -- | The type of the data source.
    sourceType :: SourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateIntegrationAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'createIntegrationAssociation_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'integrationType', 'createIntegrationAssociation_integrationType' - The type of information to be ingested.
--
-- 'integrationArn', 'createIntegrationAssociation_integrationArn' - The Amazon Resource Name (ARN) of the integration.
--
-- 'sourceApplicationUrl', 'createIntegrationAssociation_sourceApplicationUrl' - The URL for the external application.
--
-- 'sourceApplicationName', 'createIntegrationAssociation_sourceApplicationName' - The name of the external application.
--
-- 'sourceType', 'createIntegrationAssociation_sourceType' - The type of the data source.
newCreateIntegrationAssociation ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'integrationType'
  IntegrationType ->
  -- | 'integrationArn'
  Prelude.Text ->
  -- | 'sourceApplicationUrl'
  Prelude.Text ->
  -- | 'sourceApplicationName'
  Prelude.Text ->
  -- | 'sourceType'
  SourceType ->
  CreateIntegrationAssociation
newCreateIntegrationAssociation
  pInstanceId_
  pIntegrationType_
  pIntegrationArn_
  pSourceApplicationUrl_
  pSourceApplicationName_
  pSourceType_ =
    CreateIntegrationAssociation'
      { instanceId =
          pInstanceId_,
        integrationType = pIntegrationType_,
        integrationArn = pIntegrationArn_,
        sourceApplicationUrl = pSourceApplicationUrl_,
        sourceApplicationName =
          pSourceApplicationName_,
        sourceType = pSourceType_
      }

-- | The identifier of the Amazon Connect instance.
createIntegrationAssociation_instanceId :: Lens.Lens' CreateIntegrationAssociation Prelude.Text
createIntegrationAssociation_instanceId = Lens.lens (\CreateIntegrationAssociation' {instanceId} -> instanceId) (\s@CreateIntegrationAssociation' {} a -> s {instanceId = a} :: CreateIntegrationAssociation)

-- | The type of information to be ingested.
createIntegrationAssociation_integrationType :: Lens.Lens' CreateIntegrationAssociation IntegrationType
createIntegrationAssociation_integrationType = Lens.lens (\CreateIntegrationAssociation' {integrationType} -> integrationType) (\s@CreateIntegrationAssociation' {} a -> s {integrationType = a} :: CreateIntegrationAssociation)

-- | The Amazon Resource Name (ARN) of the integration.
createIntegrationAssociation_integrationArn :: Lens.Lens' CreateIntegrationAssociation Prelude.Text
createIntegrationAssociation_integrationArn = Lens.lens (\CreateIntegrationAssociation' {integrationArn} -> integrationArn) (\s@CreateIntegrationAssociation' {} a -> s {integrationArn = a} :: CreateIntegrationAssociation)

-- | The URL for the external application.
createIntegrationAssociation_sourceApplicationUrl :: Lens.Lens' CreateIntegrationAssociation Prelude.Text
createIntegrationAssociation_sourceApplicationUrl = Lens.lens (\CreateIntegrationAssociation' {sourceApplicationUrl} -> sourceApplicationUrl) (\s@CreateIntegrationAssociation' {} a -> s {sourceApplicationUrl = a} :: CreateIntegrationAssociation)

-- | The name of the external application.
createIntegrationAssociation_sourceApplicationName :: Lens.Lens' CreateIntegrationAssociation Prelude.Text
createIntegrationAssociation_sourceApplicationName = Lens.lens (\CreateIntegrationAssociation' {sourceApplicationName} -> sourceApplicationName) (\s@CreateIntegrationAssociation' {} a -> s {sourceApplicationName = a} :: CreateIntegrationAssociation)

-- | The type of the data source.
createIntegrationAssociation_sourceType :: Lens.Lens' CreateIntegrationAssociation SourceType
createIntegrationAssociation_sourceType = Lens.lens (\CreateIntegrationAssociation' {sourceType} -> sourceType) (\s@CreateIntegrationAssociation' {} a -> s {sourceType = a} :: CreateIntegrationAssociation)

instance
  Prelude.AWSRequest
    CreateIntegrationAssociation
  where
  type
    Rs CreateIntegrationAssociation =
      CreateIntegrationAssociationResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIntegrationAssociationResponse'
            Prelude.<$> (x Prelude..?> "IntegrationAssociationArn")
            Prelude.<*> (x Prelude..?> "IntegrationAssociationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateIntegrationAssociation

instance Prelude.NFData CreateIntegrationAssociation

instance
  Prelude.ToHeaders
    CreateIntegrationAssociation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateIntegrationAssociation where
  toJSON CreateIntegrationAssociation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IntegrationType" Prelude..= integrationType),
            Prelude.Just
              ("IntegrationArn" Prelude..= integrationArn),
            Prelude.Just
              ( "SourceApplicationUrl"
                  Prelude..= sourceApplicationUrl
              ),
            Prelude.Just
              ( "SourceApplicationName"
                  Prelude..= sourceApplicationName
              ),
            Prelude.Just ("SourceType" Prelude..= sourceType)
          ]
      )

instance Prelude.ToPath CreateIntegrationAssociation where
  toPath CreateIntegrationAssociation' {..} =
    Prelude.mconcat
      [ "/instance/",
        Prelude.toBS instanceId,
        "/integration-associations"
      ]

instance Prelude.ToQuery CreateIntegrationAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateIntegrationAssociationResponse' smart constructor.
data CreateIntegrationAssociationResponse = CreateIntegrationAssociationResponse'
  { -- | The Amazon Resource Name (ARN) for the association.
    integrationAssociationArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the association.
    integrationAssociationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'integrationAssociationId', 'createIntegrationAssociationResponse_integrationAssociationId' - The identifier for the association.
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

-- | The identifier for the association.
createIntegrationAssociationResponse_integrationAssociationId :: Lens.Lens' CreateIntegrationAssociationResponse (Prelude.Maybe Prelude.Text)
createIntegrationAssociationResponse_integrationAssociationId = Lens.lens (\CreateIntegrationAssociationResponse' {integrationAssociationId} -> integrationAssociationId) (\s@CreateIntegrationAssociationResponse' {} a -> s {integrationAssociationId = a} :: CreateIntegrationAssociationResponse)

-- | The response's http status code.
createIntegrationAssociationResponse_httpStatus :: Lens.Lens' CreateIntegrationAssociationResponse Prelude.Int
createIntegrationAssociationResponse_httpStatus = Lens.lens (\CreateIntegrationAssociationResponse' {httpStatus} -> httpStatus) (\s@CreateIntegrationAssociationResponse' {} a -> s {httpStatus = a} :: CreateIntegrationAssociationResponse)

instance
  Prelude.NFData
    CreateIntegrationAssociationResponse
