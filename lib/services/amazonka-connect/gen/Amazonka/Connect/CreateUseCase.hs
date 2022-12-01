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
-- Module      : Amazonka.Connect.CreateUseCase
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a use case for an integration association.
module Amazonka.Connect.CreateUseCase
  ( -- * Creating a Request
    CreateUseCase (..),
    newCreateUseCase,

    -- * Request Lenses
    createUseCase_tags,
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateUseCase' smart constructor.
data CreateUseCase = CreateUseCase'
  { -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the integration association.
    integrationAssociationId :: Prelude.Text,
    -- | The type of use case to associate to the integration association. Each
    -- integration association can have only one of each use case type.
    useCaseType :: UseCaseType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUseCase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createUseCase_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'instanceId', 'createUseCase_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'integrationAssociationId', 'createUseCase_integrationAssociationId' - The identifier for the integration association.
--
-- 'useCaseType', 'createUseCase_useCaseType' - The type of use case to associate to the integration association. Each
-- integration association can have only one of each use case type.
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
      { tags = Prelude.Nothing,
        instanceId = pInstanceId_,
        integrationAssociationId =
          pIntegrationAssociationId_,
        useCaseType = pUseCaseType_
      }

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
createUseCase_tags :: Lens.Lens' CreateUseCase (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createUseCase_tags = Lens.lens (\CreateUseCase' {tags} -> tags) (\s@CreateUseCase' {} a -> s {tags = a} :: CreateUseCase) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
createUseCase_instanceId :: Lens.Lens' CreateUseCase Prelude.Text
createUseCase_instanceId = Lens.lens (\CreateUseCase' {instanceId} -> instanceId) (\s@CreateUseCase' {} a -> s {instanceId = a} :: CreateUseCase)

-- | The identifier for the integration association.
createUseCase_integrationAssociationId :: Lens.Lens' CreateUseCase Prelude.Text
createUseCase_integrationAssociationId = Lens.lens (\CreateUseCase' {integrationAssociationId} -> integrationAssociationId) (\s@CreateUseCase' {} a -> s {integrationAssociationId = a} :: CreateUseCase)

-- | The type of use case to associate to the integration association. Each
-- integration association can have only one of each use case type.
createUseCase_useCaseType :: Lens.Lens' CreateUseCase UseCaseType
createUseCase_useCaseType = Lens.lens (\CreateUseCase' {useCaseType} -> useCaseType) (\s@CreateUseCase' {} a -> s {useCaseType = a} :: CreateUseCase)

instance Core.AWSRequest CreateUseCase where
  type
    AWSResponse CreateUseCase =
      CreateUseCaseResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUseCaseResponse'
            Prelude.<$> (x Core..?> "UseCaseArn")
            Prelude.<*> (x Core..?> "UseCaseId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUseCase where
  hashWithSalt _salt CreateUseCase' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` integrationAssociationId
      `Prelude.hashWithSalt` useCaseType

instance Prelude.NFData CreateUseCase where
  rnf CreateUseCase' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf integrationAssociationId
      `Prelude.seq` Prelude.rnf useCaseType

instance Core.ToHeaders CreateUseCase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateUseCase where
  toJSON CreateUseCase' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("UseCaseType" Core..= useCaseType)
          ]
      )

instance Core.ToPath CreateUseCase where
  toPath CreateUseCase' {..} =
    Prelude.mconcat
      [ "/instance/",
        Core.toBS instanceId,
        "/integration-associations/",
        Core.toBS integrationAssociationId,
        "/use-cases"
      ]

instance Core.ToQuery CreateUseCase where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateUseCaseResponse where
  rnf CreateUseCaseResponse' {..} =
    Prelude.rnf useCaseArn
      `Prelude.seq` Prelude.rnf useCaseId
      `Prelude.seq` Prelude.rnf httpStatus
