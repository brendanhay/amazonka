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
-- Module      : Network.AWS.Connect.CreateUseCase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Creates a use case for an AppIntegration association.
module Network.AWS.Connect.CreateUseCase
  ( -- * Creating a Request
    CreateUseCase (..),
    newCreateUseCase,

    -- * Request Lenses
    createUseCase_instanceId,
    createUseCase_integrationAssociationId,
    createUseCase_useCaseType,

    -- * Destructuring the Response
    CreateUseCaseResponse (..),
    newCreateUseCaseResponse,

    -- * Response Lenses
    createUseCaseResponse_useCaseArn,
    createUseCaseResponse_useCaseId,
    createUseCaseResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateUseCase' smart constructor.
data CreateUseCase = CreateUseCase'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The identifier for the AppIntegration association.
    integrationAssociationId :: Core.Text,
    -- | The type of use case to associate to the AppIntegration association.
    -- Each AppIntegration association can have only one of each use case type.
    useCaseType :: UseCaseType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUseCase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'createUseCase_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'integrationAssociationId', 'createUseCase_integrationAssociationId' - The identifier for the AppIntegration association.
--
-- 'useCaseType', 'createUseCase_useCaseType' - The type of use case to associate to the AppIntegration association.
-- Each AppIntegration association can have only one of each use case type.
newCreateUseCase ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'integrationAssociationId'
  Core.Text ->
  -- | 'useCaseType'
  UseCaseType ->
  CreateUseCase
newCreateUseCase
  pInstanceId_
  pIntegrationAssociationId_
  pUseCaseType_ =
    CreateUseCase'
      { instanceId = pInstanceId_,
        integrationAssociationId =
          pIntegrationAssociationId_,
        useCaseType = pUseCaseType_
      }

-- | The identifier of the Amazon Connect instance.
createUseCase_instanceId :: Lens.Lens' CreateUseCase Core.Text
createUseCase_instanceId = Lens.lens (\CreateUseCase' {instanceId} -> instanceId) (\s@CreateUseCase' {} a -> s {instanceId = a} :: CreateUseCase)

-- | The identifier for the AppIntegration association.
createUseCase_integrationAssociationId :: Lens.Lens' CreateUseCase Core.Text
createUseCase_integrationAssociationId = Lens.lens (\CreateUseCase' {integrationAssociationId} -> integrationAssociationId) (\s@CreateUseCase' {} a -> s {integrationAssociationId = a} :: CreateUseCase)

-- | The type of use case to associate to the AppIntegration association.
-- Each AppIntegration association can have only one of each use case type.
createUseCase_useCaseType :: Lens.Lens' CreateUseCase UseCaseType
createUseCase_useCaseType = Lens.lens (\CreateUseCase' {useCaseType} -> useCaseType) (\s@CreateUseCase' {} a -> s {useCaseType = a} :: CreateUseCase)

instance Core.AWSRequest CreateUseCase where
  type
    AWSResponse CreateUseCase =
      CreateUseCaseResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUseCaseResponse'
            Core.<$> (x Core..?> "UseCaseArn")
            Core.<*> (x Core..?> "UseCaseId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateUseCase

instance Core.NFData CreateUseCase

instance Core.ToHeaders CreateUseCase where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateUseCase where
  toJSON CreateUseCase' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("UseCaseType" Core..= useCaseType)]
      )

instance Core.ToPath CreateUseCase where
  toPath CreateUseCase' {..} =
    Core.mconcat
      [ "/instance/",
        Core.toBS instanceId,
        "/integration-associations/",
        Core.toBS integrationAssociationId,
        "/use-cases"
      ]

instance Core.ToQuery CreateUseCase where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateUseCaseResponse' smart constructor.
data CreateUseCaseResponse = CreateUseCaseResponse'
  { -- | The Amazon Resource Name (ARN) for the use case.
    useCaseArn :: Core.Maybe Core.Text,
    -- | The identifier of the use case.
    useCaseId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUseCaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'useCaseArn', 'createUseCaseResponse_useCaseArn' - The Amazon Resource Name (ARN) for the use case.
--
-- 'useCaseId', 'createUseCaseResponse_useCaseId' - The identifier of the use case.
--
-- 'httpStatus', 'createUseCaseResponse_httpStatus' - The response's http status code.
newCreateUseCaseResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateUseCaseResponse
newCreateUseCaseResponse pHttpStatus_ =
  CreateUseCaseResponse'
    { useCaseArn = Core.Nothing,
      useCaseId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the use case.
createUseCaseResponse_useCaseArn :: Lens.Lens' CreateUseCaseResponse (Core.Maybe Core.Text)
createUseCaseResponse_useCaseArn = Lens.lens (\CreateUseCaseResponse' {useCaseArn} -> useCaseArn) (\s@CreateUseCaseResponse' {} a -> s {useCaseArn = a} :: CreateUseCaseResponse)

-- | The identifier of the use case.
createUseCaseResponse_useCaseId :: Lens.Lens' CreateUseCaseResponse (Core.Maybe Core.Text)
createUseCaseResponse_useCaseId = Lens.lens (\CreateUseCaseResponse' {useCaseId} -> useCaseId) (\s@CreateUseCaseResponse' {} a -> s {useCaseId = a} :: CreateUseCaseResponse)

-- | The response's http status code.
createUseCaseResponse_httpStatus :: Lens.Lens' CreateUseCaseResponse Core.Int
createUseCaseResponse_httpStatus = Lens.lens (\CreateUseCaseResponse' {httpStatus} -> httpStatus) (\s@CreateUseCaseResponse' {} a -> s {httpStatus = a} :: CreateUseCaseResponse)

instance Core.NFData CreateUseCaseResponse
