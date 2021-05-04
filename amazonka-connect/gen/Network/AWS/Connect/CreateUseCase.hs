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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateUseCase' smart constructor.
data CreateUseCase = CreateUseCase'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the AppIntegration association.
    integrationAssociationId :: Prelude.Text,
    -- | The type of use case to associate to the AppIntegration association.
    -- Each AppIntegration association can have only one of each use case type.
    useCaseType :: UseCaseType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'integrationAssociationId'
  Prelude.Text ->
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
createUseCase_instanceId :: Lens.Lens' CreateUseCase Prelude.Text
createUseCase_instanceId = Lens.lens (\CreateUseCase' {instanceId} -> instanceId) (\s@CreateUseCase' {} a -> s {instanceId = a} :: CreateUseCase)

-- | The identifier for the AppIntegration association.
createUseCase_integrationAssociationId :: Lens.Lens' CreateUseCase Prelude.Text
createUseCase_integrationAssociationId = Lens.lens (\CreateUseCase' {integrationAssociationId} -> integrationAssociationId) (\s@CreateUseCase' {} a -> s {integrationAssociationId = a} :: CreateUseCase)

-- | The type of use case to associate to the AppIntegration association.
-- Each AppIntegration association can have only one of each use case type.
createUseCase_useCaseType :: Lens.Lens' CreateUseCase UseCaseType
createUseCase_useCaseType = Lens.lens (\CreateUseCase' {useCaseType} -> useCaseType) (\s@CreateUseCase' {} a -> s {useCaseType = a} :: CreateUseCase)

instance Prelude.AWSRequest CreateUseCase where
  type Rs CreateUseCase = CreateUseCaseResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUseCaseResponse'
            Prelude.<$> (x Prelude..?> "UseCaseArn")
            Prelude.<*> (x Prelude..?> "UseCaseId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUseCase

instance Prelude.NFData CreateUseCase

instance Prelude.ToHeaders CreateUseCase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateUseCase where
  toJSON CreateUseCase' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("UseCaseType" Prelude..= useCaseType)
          ]
      )

instance Prelude.ToPath CreateUseCase where
  toPath CreateUseCase' {..} =
    Prelude.mconcat
      [ "/instance/",
        Prelude.toBS instanceId,
        "/integration-associations/",
        Prelude.toBS integrationAssociationId,
        "/use-cases"
      ]

instance Prelude.ToQuery CreateUseCase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUseCaseResponse' smart constructor.
data CreateUseCaseResponse = CreateUseCaseResponse'
  { -- | The Amazon Resource Name (ARN) for the use case.
    useCaseArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the use case.
    useCaseId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateUseCaseResponse
newCreateUseCaseResponse pHttpStatus_ =
  CreateUseCaseResponse'
    { useCaseArn =
        Prelude.Nothing,
      useCaseId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the use case.
createUseCaseResponse_useCaseArn :: Lens.Lens' CreateUseCaseResponse (Prelude.Maybe Prelude.Text)
createUseCaseResponse_useCaseArn = Lens.lens (\CreateUseCaseResponse' {useCaseArn} -> useCaseArn) (\s@CreateUseCaseResponse' {} a -> s {useCaseArn = a} :: CreateUseCaseResponse)

-- | The identifier of the use case.
createUseCaseResponse_useCaseId :: Lens.Lens' CreateUseCaseResponse (Prelude.Maybe Prelude.Text)
createUseCaseResponse_useCaseId = Lens.lens (\CreateUseCaseResponse' {useCaseId} -> useCaseId) (\s@CreateUseCaseResponse' {} a -> s {useCaseId = a} :: CreateUseCaseResponse)

-- | The response's http status code.
createUseCaseResponse_httpStatus :: Lens.Lens' CreateUseCaseResponse Prelude.Int
createUseCaseResponse_httpStatus = Lens.lens (\CreateUseCaseResponse' {httpStatus} -> httpStatus) (\s@CreateUseCaseResponse' {} a -> s {httpStatus = a} :: CreateUseCaseResponse)

instance Prelude.NFData CreateUseCaseResponse
