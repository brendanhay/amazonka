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
-- Module      : Amazonka.IoTWireless.CreateWirelessGatewayTaskDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a gateway task definition.
module Amazonka.IoTWireless.CreateWirelessGatewayTaskDefinition
  ( -- * Creating a Request
    CreateWirelessGatewayTaskDefinition (..),
    newCreateWirelessGatewayTaskDefinition,

    -- * Request Lenses
    createWirelessGatewayTaskDefinition_clientRequestToken,
    createWirelessGatewayTaskDefinition_name,
    createWirelessGatewayTaskDefinition_tags,
    createWirelessGatewayTaskDefinition_update,
    createWirelessGatewayTaskDefinition_autoCreateTasks,

    -- * Destructuring the Response
    CreateWirelessGatewayTaskDefinitionResponse (..),
    newCreateWirelessGatewayTaskDefinitionResponse,

    -- * Response Lenses
    createWirelessGatewayTaskDefinitionResponse_arn,
    createWirelessGatewayTaskDefinitionResponse_id,
    createWirelessGatewayTaskDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWirelessGatewayTaskDefinition' smart constructor.
data CreateWirelessGatewayTaskDefinition = CreateWirelessGatewayTaskDefinition'
  { -- | Each resource must have a unique client request token. If you try to
    -- create a new resource with the same token as a resource that already
    -- exists, an exception occurs. If you omit this value, AWS SDKs will
    -- automatically generate a unique client request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the new resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The tags to attach to the specified resource. Tags are metadata that you
    -- can use to manage a resource.
    tags :: Prelude.Maybe [Tag],
    -- | Information about the gateways to update.
    update :: Prelude.Maybe UpdateWirelessGatewayTaskCreate,
    -- | Whether to automatically create tasks using this task definition for all
    -- gateways with the specified current version. If @false@, the task must
    -- me created by calling @CreateWirelessGatewayTask@.
    autoCreateTasks :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWirelessGatewayTaskDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createWirelessGatewayTaskDefinition_clientRequestToken' - Each resource must have a unique client request token. If you try to
-- create a new resource with the same token as a resource that already
-- exists, an exception occurs. If you omit this value, AWS SDKs will
-- automatically generate a unique client request.
--
-- 'name', 'createWirelessGatewayTaskDefinition_name' - The name of the new resource.
--
-- 'tags', 'createWirelessGatewayTaskDefinition_tags' - The tags to attach to the specified resource. Tags are metadata that you
-- can use to manage a resource.
--
-- 'update', 'createWirelessGatewayTaskDefinition_update' - Information about the gateways to update.
--
-- 'autoCreateTasks', 'createWirelessGatewayTaskDefinition_autoCreateTasks' - Whether to automatically create tasks using this task definition for all
-- gateways with the specified current version. If @false@, the task must
-- me created by calling @CreateWirelessGatewayTask@.
newCreateWirelessGatewayTaskDefinition ::
  -- | 'autoCreateTasks'
  Prelude.Bool ->
  CreateWirelessGatewayTaskDefinition
newCreateWirelessGatewayTaskDefinition
  pAutoCreateTasks_ =
    CreateWirelessGatewayTaskDefinition'
      { clientRequestToken =
          Prelude.Nothing,
        name = Prelude.Nothing,
        tags = Prelude.Nothing,
        update = Prelude.Nothing,
        autoCreateTasks = pAutoCreateTasks_
      }

-- | Each resource must have a unique client request token. If you try to
-- create a new resource with the same token as a resource that already
-- exists, an exception occurs. If you omit this value, AWS SDKs will
-- automatically generate a unique client request.
createWirelessGatewayTaskDefinition_clientRequestToken :: Lens.Lens' CreateWirelessGatewayTaskDefinition (Prelude.Maybe Prelude.Text)
createWirelessGatewayTaskDefinition_clientRequestToken = Lens.lens (\CreateWirelessGatewayTaskDefinition' {clientRequestToken} -> clientRequestToken) (\s@CreateWirelessGatewayTaskDefinition' {} a -> s {clientRequestToken = a} :: CreateWirelessGatewayTaskDefinition)

-- | The name of the new resource.
createWirelessGatewayTaskDefinition_name :: Lens.Lens' CreateWirelessGatewayTaskDefinition (Prelude.Maybe Prelude.Text)
createWirelessGatewayTaskDefinition_name = Lens.lens (\CreateWirelessGatewayTaskDefinition' {name} -> name) (\s@CreateWirelessGatewayTaskDefinition' {} a -> s {name = a} :: CreateWirelessGatewayTaskDefinition)

-- | The tags to attach to the specified resource. Tags are metadata that you
-- can use to manage a resource.
createWirelessGatewayTaskDefinition_tags :: Lens.Lens' CreateWirelessGatewayTaskDefinition (Prelude.Maybe [Tag])
createWirelessGatewayTaskDefinition_tags = Lens.lens (\CreateWirelessGatewayTaskDefinition' {tags} -> tags) (\s@CreateWirelessGatewayTaskDefinition' {} a -> s {tags = a} :: CreateWirelessGatewayTaskDefinition) Prelude.. Lens.mapping Lens.coerced

-- | Information about the gateways to update.
createWirelessGatewayTaskDefinition_update :: Lens.Lens' CreateWirelessGatewayTaskDefinition (Prelude.Maybe UpdateWirelessGatewayTaskCreate)
createWirelessGatewayTaskDefinition_update = Lens.lens (\CreateWirelessGatewayTaskDefinition' {update} -> update) (\s@CreateWirelessGatewayTaskDefinition' {} a -> s {update = a} :: CreateWirelessGatewayTaskDefinition)

-- | Whether to automatically create tasks using this task definition for all
-- gateways with the specified current version. If @false@, the task must
-- me created by calling @CreateWirelessGatewayTask@.
createWirelessGatewayTaskDefinition_autoCreateTasks :: Lens.Lens' CreateWirelessGatewayTaskDefinition Prelude.Bool
createWirelessGatewayTaskDefinition_autoCreateTasks = Lens.lens (\CreateWirelessGatewayTaskDefinition' {autoCreateTasks} -> autoCreateTasks) (\s@CreateWirelessGatewayTaskDefinition' {} a -> s {autoCreateTasks = a} :: CreateWirelessGatewayTaskDefinition)

instance
  Core.AWSRequest
    CreateWirelessGatewayTaskDefinition
  where
  type
    AWSResponse CreateWirelessGatewayTaskDefinition =
      CreateWirelessGatewayTaskDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWirelessGatewayTaskDefinitionResponse'
            Prelude.<$> (x Data..?> "Arn") Prelude.<*> (x Data..?> "Id")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateWirelessGatewayTaskDefinition
  where
  hashWithSalt
    _salt
    CreateWirelessGatewayTaskDefinition' {..} =
      _salt `Prelude.hashWithSalt` clientRequestToken
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` update
        `Prelude.hashWithSalt` autoCreateTasks

instance
  Prelude.NFData
    CreateWirelessGatewayTaskDefinition
  where
  rnf CreateWirelessGatewayTaskDefinition' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf update
      `Prelude.seq` Prelude.rnf autoCreateTasks

instance
  Data.ToHeaders
    CreateWirelessGatewayTaskDefinition
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    CreateWirelessGatewayTaskDefinition
  where
  toJSON CreateWirelessGatewayTaskDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Name" Data..=) Prelude.<$> name,
            ("Tags" Data..=) Prelude.<$> tags,
            ("Update" Data..=) Prelude.<$> update,
            Prelude.Just
              ("AutoCreateTasks" Data..= autoCreateTasks)
          ]
      )

instance
  Data.ToPath
    CreateWirelessGatewayTaskDefinition
  where
  toPath =
    Prelude.const "/wireless-gateway-task-definitions"

instance
  Data.ToQuery
    CreateWirelessGatewayTaskDefinition
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWirelessGatewayTaskDefinitionResponse' smart constructor.
data CreateWirelessGatewayTaskDefinitionResponse = CreateWirelessGatewayTaskDefinitionResponse'
  { -- | The Amazon Resource Name of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the new wireless gateway task definition.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWirelessGatewayTaskDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createWirelessGatewayTaskDefinitionResponse_arn' - The Amazon Resource Name of the resource.
--
-- 'id', 'createWirelessGatewayTaskDefinitionResponse_id' - The ID of the new wireless gateway task definition.
--
-- 'httpStatus', 'createWirelessGatewayTaskDefinitionResponse_httpStatus' - The response's http status code.
newCreateWirelessGatewayTaskDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWirelessGatewayTaskDefinitionResponse
newCreateWirelessGatewayTaskDefinitionResponse
  pHttpStatus_ =
    CreateWirelessGatewayTaskDefinitionResponse'
      { arn =
          Prelude.Nothing,
        id = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name of the resource.
createWirelessGatewayTaskDefinitionResponse_arn :: Lens.Lens' CreateWirelessGatewayTaskDefinitionResponse (Prelude.Maybe Prelude.Text)
createWirelessGatewayTaskDefinitionResponse_arn = Lens.lens (\CreateWirelessGatewayTaskDefinitionResponse' {arn} -> arn) (\s@CreateWirelessGatewayTaskDefinitionResponse' {} a -> s {arn = a} :: CreateWirelessGatewayTaskDefinitionResponse)

-- | The ID of the new wireless gateway task definition.
createWirelessGatewayTaskDefinitionResponse_id :: Lens.Lens' CreateWirelessGatewayTaskDefinitionResponse (Prelude.Maybe Prelude.Text)
createWirelessGatewayTaskDefinitionResponse_id = Lens.lens (\CreateWirelessGatewayTaskDefinitionResponse' {id} -> id) (\s@CreateWirelessGatewayTaskDefinitionResponse' {} a -> s {id = a} :: CreateWirelessGatewayTaskDefinitionResponse)

-- | The response's http status code.
createWirelessGatewayTaskDefinitionResponse_httpStatus :: Lens.Lens' CreateWirelessGatewayTaskDefinitionResponse Prelude.Int
createWirelessGatewayTaskDefinitionResponse_httpStatus = Lens.lens (\CreateWirelessGatewayTaskDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateWirelessGatewayTaskDefinitionResponse' {} a -> s {httpStatus = a} :: CreateWirelessGatewayTaskDefinitionResponse)

instance
  Prelude.NFData
    CreateWirelessGatewayTaskDefinitionResponse
  where
  rnf CreateWirelessGatewayTaskDefinitionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus
