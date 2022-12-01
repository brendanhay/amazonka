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
-- Module      : Amazonka.MigrationHubReFactorSpaces.CreateEnvironment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Web Services Migration Hub Refactor Spaces
-- environment. The caller owns the environment resource, and all Refactor
-- Spaces applications, services, and routes created within the
-- environment. They are referred to as the /environment owner/. The
-- environment owner has cross-account visibility and control of Refactor
-- Spaces resources that are added to the environment by other accounts
-- that the environment is shared with. When creating an environment,
-- Refactor Spaces provisions a transit gateway in your account.
module Amazonka.MigrationHubReFactorSpaces.CreateEnvironment
  ( -- * Creating a Request
    CreateEnvironment (..),
    newCreateEnvironment,

    -- * Request Lenses
    createEnvironment_tags,
    createEnvironment_clientToken,
    createEnvironment_description,
    createEnvironment_name,
    createEnvironment_networkFabricType,

    -- * Destructuring the Response
    CreateEnvironmentResponse (..),
    newCreateEnvironmentResponse,

    -- * Response Lenses
    createEnvironmentResponse_tags,
    createEnvironmentResponse_name,
    createEnvironmentResponse_createdTime,
    createEnvironmentResponse_arn,
    createEnvironmentResponse_state,
    createEnvironmentResponse_lastUpdatedTime,
    createEnvironmentResponse_description,
    createEnvironmentResponse_ownerAccountId,
    createEnvironmentResponse_environmentId,
    createEnvironmentResponse_networkFabricType,
    createEnvironmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubReFactorSpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEnvironment' smart constructor.
data CreateEnvironment = CreateEnvironment'
  { -- | The tags to assign to the environment. A tag is a label that you assign
    -- to an Amazon Web Services resource. Each tag consists of a key-value
    -- pair.
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the environment.
    name :: Prelude.Text,
    -- | The network fabric type of the environment.
    networkFabricType :: NetworkFabricType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createEnvironment_tags' - The tags to assign to the environment. A tag is a label that you assign
-- to an Amazon Web Services resource. Each tag consists of a key-value
-- pair.
--
-- 'clientToken', 'createEnvironment_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'description', 'createEnvironment_description' - The description of the environment.
--
-- 'name', 'createEnvironment_name' - The name of the environment.
--
-- 'networkFabricType', 'createEnvironment_networkFabricType' - The network fabric type of the environment.
newCreateEnvironment ::
  -- | 'name'
  Prelude.Text ->
  -- | 'networkFabricType'
  NetworkFabricType ->
  CreateEnvironment
newCreateEnvironment pName_ pNetworkFabricType_ =
  CreateEnvironment'
    { tags = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_,
      networkFabricType = pNetworkFabricType_
    }

-- | The tags to assign to the environment. A tag is a label that you assign
-- to an Amazon Web Services resource. Each tag consists of a key-value
-- pair.
createEnvironment_tags :: Lens.Lens' CreateEnvironment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createEnvironment_tags = Lens.lens (\CreateEnvironment' {tags} -> tags) (\s@CreateEnvironment' {} a -> s {tags = a} :: CreateEnvironment) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createEnvironment_clientToken :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_clientToken = Lens.lens (\CreateEnvironment' {clientToken} -> clientToken) (\s@CreateEnvironment' {} a -> s {clientToken = a} :: CreateEnvironment)

-- | The description of the environment.
createEnvironment_description :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_description = Lens.lens (\CreateEnvironment' {description} -> description) (\s@CreateEnvironment' {} a -> s {description = a} :: CreateEnvironment)

-- | The name of the environment.
createEnvironment_name :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_name = Lens.lens (\CreateEnvironment' {name} -> name) (\s@CreateEnvironment' {} a -> s {name = a} :: CreateEnvironment)

-- | The network fabric type of the environment.
createEnvironment_networkFabricType :: Lens.Lens' CreateEnvironment NetworkFabricType
createEnvironment_networkFabricType = Lens.lens (\CreateEnvironment' {networkFabricType} -> networkFabricType) (\s@CreateEnvironment' {} a -> s {networkFabricType = a} :: CreateEnvironment)

instance Core.AWSRequest CreateEnvironment where
  type
    AWSResponse CreateEnvironment =
      CreateEnvironmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEnvironmentResponse'
            Prelude.<$> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "CreatedTime")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "State")
            Prelude.<*> (x Core..?> "LastUpdatedTime")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "OwnerAccountId")
            Prelude.<*> (x Core..?> "EnvironmentId")
            Prelude.<*> (x Core..?> "NetworkFabricType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEnvironment where
  hashWithSalt _salt CreateEnvironment' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` networkFabricType

instance Prelude.NFData CreateEnvironment where
  rnf CreateEnvironment' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf networkFabricType

instance Core.ToHeaders CreateEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateEnvironment where
  toJSON CreateEnvironment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("ClientToken" Core..=) Prelude.<$> clientToken,
            ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just
              ("NetworkFabricType" Core..= networkFabricType)
          ]
      )

instance Core.ToPath CreateEnvironment where
  toPath = Prelude.const "/environments"

instance Core.ToQuery CreateEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEnvironmentResponse' smart constructor.
data CreateEnvironmentResponse = CreateEnvironmentResponse'
  { -- | The tags assigned to the created environment. A tag is a label that you
    -- assign to an Amazon Web Services resource. Each tag consists of a
    -- key-value pair..
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The name of the environment.
    name :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates when the environment is created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the environment.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The current state of the environment.
    state :: Prelude.Maybe EnvironmentState,
    -- | A timestamp that indicates when the environment was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | A description of the environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of environment owner.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The network fabric type of the environment.
    networkFabricType :: Prelude.Maybe NetworkFabricType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createEnvironmentResponse_tags' - The tags assigned to the created environment. A tag is a label that you
-- assign to an Amazon Web Services resource. Each tag consists of a
-- key-value pair..
--
-- 'name', 'createEnvironmentResponse_name' - The name of the environment.
--
-- 'createdTime', 'createEnvironmentResponse_createdTime' - A timestamp that indicates when the environment is created.
--
-- 'arn', 'createEnvironmentResponse_arn' - The Amazon Resource Name (ARN) of the environment.
--
-- 'state', 'createEnvironmentResponse_state' - The current state of the environment.
--
-- 'lastUpdatedTime', 'createEnvironmentResponse_lastUpdatedTime' - A timestamp that indicates when the environment was last updated.
--
-- 'description', 'createEnvironmentResponse_description' - A description of the environment.
--
-- 'ownerAccountId', 'createEnvironmentResponse_ownerAccountId' - The Amazon Web Services account ID of environment owner.
--
-- 'environmentId', 'createEnvironmentResponse_environmentId' - The unique identifier of the environment.
--
-- 'networkFabricType', 'createEnvironmentResponse_networkFabricType' - The network fabric type of the environment.
--
-- 'httpStatus', 'createEnvironmentResponse_httpStatus' - The response's http status code.
newCreateEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEnvironmentResponse
newCreateEnvironmentResponse pHttpStatus_ =
  CreateEnvironmentResponse'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      description = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      networkFabricType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tags assigned to the created environment. A tag is a label that you
-- assign to an Amazon Web Services resource. Each tag consists of a
-- key-value pair..
createEnvironmentResponse_tags :: Lens.Lens' CreateEnvironmentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createEnvironmentResponse_tags = Lens.lens (\CreateEnvironmentResponse' {tags} -> tags) (\s@CreateEnvironmentResponse' {} a -> s {tags = a} :: CreateEnvironmentResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The name of the environment.
createEnvironmentResponse_name :: Lens.Lens' CreateEnvironmentResponse (Prelude.Maybe Prelude.Text)
createEnvironmentResponse_name = Lens.lens (\CreateEnvironmentResponse' {name} -> name) (\s@CreateEnvironmentResponse' {} a -> s {name = a} :: CreateEnvironmentResponse)

-- | A timestamp that indicates when the environment is created.
createEnvironmentResponse_createdTime :: Lens.Lens' CreateEnvironmentResponse (Prelude.Maybe Prelude.UTCTime)
createEnvironmentResponse_createdTime = Lens.lens (\CreateEnvironmentResponse' {createdTime} -> createdTime) (\s@CreateEnvironmentResponse' {} a -> s {createdTime = a} :: CreateEnvironmentResponse) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the environment.
createEnvironmentResponse_arn :: Lens.Lens' CreateEnvironmentResponse (Prelude.Maybe Prelude.Text)
createEnvironmentResponse_arn = Lens.lens (\CreateEnvironmentResponse' {arn} -> arn) (\s@CreateEnvironmentResponse' {} a -> s {arn = a} :: CreateEnvironmentResponse)

-- | The current state of the environment.
createEnvironmentResponse_state :: Lens.Lens' CreateEnvironmentResponse (Prelude.Maybe EnvironmentState)
createEnvironmentResponse_state = Lens.lens (\CreateEnvironmentResponse' {state} -> state) (\s@CreateEnvironmentResponse' {} a -> s {state = a} :: CreateEnvironmentResponse)

-- | A timestamp that indicates when the environment was last updated.
createEnvironmentResponse_lastUpdatedTime :: Lens.Lens' CreateEnvironmentResponse (Prelude.Maybe Prelude.UTCTime)
createEnvironmentResponse_lastUpdatedTime = Lens.lens (\CreateEnvironmentResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@CreateEnvironmentResponse' {} a -> s {lastUpdatedTime = a} :: CreateEnvironmentResponse) Prelude.. Lens.mapping Core._Time

-- | A description of the environment.
createEnvironmentResponse_description :: Lens.Lens' CreateEnvironmentResponse (Prelude.Maybe Prelude.Text)
createEnvironmentResponse_description = Lens.lens (\CreateEnvironmentResponse' {description} -> description) (\s@CreateEnvironmentResponse' {} a -> s {description = a} :: CreateEnvironmentResponse)

-- | The Amazon Web Services account ID of environment owner.
createEnvironmentResponse_ownerAccountId :: Lens.Lens' CreateEnvironmentResponse (Prelude.Maybe Prelude.Text)
createEnvironmentResponse_ownerAccountId = Lens.lens (\CreateEnvironmentResponse' {ownerAccountId} -> ownerAccountId) (\s@CreateEnvironmentResponse' {} a -> s {ownerAccountId = a} :: CreateEnvironmentResponse)

-- | The unique identifier of the environment.
createEnvironmentResponse_environmentId :: Lens.Lens' CreateEnvironmentResponse (Prelude.Maybe Prelude.Text)
createEnvironmentResponse_environmentId = Lens.lens (\CreateEnvironmentResponse' {environmentId} -> environmentId) (\s@CreateEnvironmentResponse' {} a -> s {environmentId = a} :: CreateEnvironmentResponse)

-- | The network fabric type of the environment.
createEnvironmentResponse_networkFabricType :: Lens.Lens' CreateEnvironmentResponse (Prelude.Maybe NetworkFabricType)
createEnvironmentResponse_networkFabricType = Lens.lens (\CreateEnvironmentResponse' {networkFabricType} -> networkFabricType) (\s@CreateEnvironmentResponse' {} a -> s {networkFabricType = a} :: CreateEnvironmentResponse)

-- | The response's http status code.
createEnvironmentResponse_httpStatus :: Lens.Lens' CreateEnvironmentResponse Prelude.Int
createEnvironmentResponse_httpStatus = Lens.lens (\CreateEnvironmentResponse' {httpStatus} -> httpStatus) (\s@CreateEnvironmentResponse' {} a -> s {httpStatus = a} :: CreateEnvironmentResponse)

instance Prelude.NFData CreateEnvironmentResponse where
  rnf CreateEnvironmentResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf networkFabricType
      `Prelude.seq` Prelude.rnf httpStatus
