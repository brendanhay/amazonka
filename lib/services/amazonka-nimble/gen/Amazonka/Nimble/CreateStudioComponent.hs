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
-- Module      : Amazonka.Nimble.CreateStudioComponent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a studio component resource.
module Amazonka.Nimble.CreateStudioComponent
  ( -- * Creating a Request
    CreateStudioComponent (..),
    newCreateStudioComponent,

    -- * Request Lenses
    createStudioComponent_initializationScripts,
    createStudioComponent_clientToken,
    createStudioComponent_ec2SecurityGroupIds,
    createStudioComponent_subtype,
    createStudioComponent_scriptParameters,
    createStudioComponent_configuration,
    createStudioComponent_description,
    createStudioComponent_tags,
    createStudioComponent_studioId,
    createStudioComponent_name,
    createStudioComponent_type,

    -- * Destructuring the Response
    CreateStudioComponentResponse (..),
    newCreateStudioComponentResponse,

    -- * Response Lenses
    createStudioComponentResponse_studioComponent,
    createStudioComponentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The studio components.
--
-- /See:/ 'newCreateStudioComponent' smart constructor.
data CreateStudioComponent = CreateStudioComponent'
  { -- | Initialization scripts for studio components.
    initializationScripts :: Prelude.Maybe [StudioComponentInitializationScript],
    -- | To make an idempotent API request using one of these actions, specify a
    -- client token in the request. You should not reuse the same client token
    -- for other API requests. If you retry a request that completed
    -- successfully using the same client token and the same parameters, the
    -- retry succeeds without performing any further actions. If you retry a
    -- successful request using the same client token, but one or more of the
    -- parameters are different, the retry fails with a ValidationException
    -- error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The EC2 security groups that control access to the studio component.
    ec2SecurityGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The specific subtype of a studio component.
    subtype :: Prelude.Maybe StudioComponentSubtype,
    -- | Parameters for the studio component scripts.
    scriptParameters :: Prelude.Maybe [ScriptParameterKeyValue],
    -- | The configuration of the studio component, based on component type.
    configuration :: Prelude.Maybe StudioComponentConfiguration,
    -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | A collection of labels, in the form of key:value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The studio ID.
    studioId :: Prelude.Text,
    -- | The name for the studio component.
    name :: Prelude.Text,
    -- | The type of the studio component.
    type' :: StudioComponentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStudioComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initializationScripts', 'createStudioComponent_initializationScripts' - Initialization scripts for studio components.
--
-- 'clientToken', 'createStudioComponent_clientToken' - To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
--
-- 'ec2SecurityGroupIds', 'createStudioComponent_ec2SecurityGroupIds' - The EC2 security groups that control access to the studio component.
--
-- 'subtype', 'createStudioComponent_subtype' - The specific subtype of a studio component.
--
-- 'scriptParameters', 'createStudioComponent_scriptParameters' - Parameters for the studio component scripts.
--
-- 'configuration', 'createStudioComponent_configuration' - The configuration of the studio component, based on component type.
--
-- 'description', 'createStudioComponent_description' - The description.
--
-- 'tags', 'createStudioComponent_tags' - A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
--
-- 'studioId', 'createStudioComponent_studioId' - The studio ID.
--
-- 'name', 'createStudioComponent_name' - The name for the studio component.
--
-- 'type'', 'createStudioComponent_type' - The type of the studio component.
newCreateStudioComponent ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  StudioComponentType ->
  CreateStudioComponent
newCreateStudioComponent pStudioId_ pName_ pType_ =
  CreateStudioComponent'
    { initializationScripts =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      ec2SecurityGroupIds = Prelude.Nothing,
      subtype = Prelude.Nothing,
      scriptParameters = Prelude.Nothing,
      configuration = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      studioId = pStudioId_,
      name = pName_,
      type' = pType_
    }

-- | Initialization scripts for studio components.
createStudioComponent_initializationScripts :: Lens.Lens' CreateStudioComponent (Prelude.Maybe [StudioComponentInitializationScript])
createStudioComponent_initializationScripts = Lens.lens (\CreateStudioComponent' {initializationScripts} -> initializationScripts) (\s@CreateStudioComponent' {} a -> s {initializationScripts = a} :: CreateStudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
createStudioComponent_clientToken :: Lens.Lens' CreateStudioComponent (Prelude.Maybe Prelude.Text)
createStudioComponent_clientToken = Lens.lens (\CreateStudioComponent' {clientToken} -> clientToken) (\s@CreateStudioComponent' {} a -> s {clientToken = a} :: CreateStudioComponent)

-- | The EC2 security groups that control access to the studio component.
createStudioComponent_ec2SecurityGroupIds :: Lens.Lens' CreateStudioComponent (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createStudioComponent_ec2SecurityGroupIds = Lens.lens (\CreateStudioComponent' {ec2SecurityGroupIds} -> ec2SecurityGroupIds) (\s@CreateStudioComponent' {} a -> s {ec2SecurityGroupIds = a} :: CreateStudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | The specific subtype of a studio component.
createStudioComponent_subtype :: Lens.Lens' CreateStudioComponent (Prelude.Maybe StudioComponentSubtype)
createStudioComponent_subtype = Lens.lens (\CreateStudioComponent' {subtype} -> subtype) (\s@CreateStudioComponent' {} a -> s {subtype = a} :: CreateStudioComponent)

-- | Parameters for the studio component scripts.
createStudioComponent_scriptParameters :: Lens.Lens' CreateStudioComponent (Prelude.Maybe [ScriptParameterKeyValue])
createStudioComponent_scriptParameters = Lens.lens (\CreateStudioComponent' {scriptParameters} -> scriptParameters) (\s@CreateStudioComponent' {} a -> s {scriptParameters = a} :: CreateStudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | The configuration of the studio component, based on component type.
createStudioComponent_configuration :: Lens.Lens' CreateStudioComponent (Prelude.Maybe StudioComponentConfiguration)
createStudioComponent_configuration = Lens.lens (\CreateStudioComponent' {configuration} -> configuration) (\s@CreateStudioComponent' {} a -> s {configuration = a} :: CreateStudioComponent)

-- | The description.
createStudioComponent_description :: Lens.Lens' CreateStudioComponent (Prelude.Maybe Prelude.Text)
createStudioComponent_description = Lens.lens (\CreateStudioComponent' {description} -> description) (\s@CreateStudioComponent' {} a -> s {description = a} :: CreateStudioComponent)

-- | A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
createStudioComponent_tags :: Lens.Lens' CreateStudioComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStudioComponent_tags = Lens.lens (\CreateStudioComponent' {tags} -> tags) (\s@CreateStudioComponent' {} a -> s {tags = a} :: CreateStudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | The studio ID.
createStudioComponent_studioId :: Lens.Lens' CreateStudioComponent Prelude.Text
createStudioComponent_studioId = Lens.lens (\CreateStudioComponent' {studioId} -> studioId) (\s@CreateStudioComponent' {} a -> s {studioId = a} :: CreateStudioComponent)

-- | The name for the studio component.
createStudioComponent_name :: Lens.Lens' CreateStudioComponent Prelude.Text
createStudioComponent_name = Lens.lens (\CreateStudioComponent' {name} -> name) (\s@CreateStudioComponent' {} a -> s {name = a} :: CreateStudioComponent)

-- | The type of the studio component.
createStudioComponent_type :: Lens.Lens' CreateStudioComponent StudioComponentType
createStudioComponent_type = Lens.lens (\CreateStudioComponent' {type'} -> type') (\s@CreateStudioComponent' {} a -> s {type' = a} :: CreateStudioComponent)

instance Core.AWSRequest CreateStudioComponent where
  type
    AWSResponse CreateStudioComponent =
      CreateStudioComponentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStudioComponentResponse'
            Prelude.<$> (x Core..?> "studioComponent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStudioComponent

instance Prelude.NFData CreateStudioComponent

instance Core.ToHeaders CreateStudioComponent where
  toHeaders CreateStudioComponent' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Core.=# clientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON CreateStudioComponent where
  toJSON CreateStudioComponent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("initializationScripts" Core..=)
              Prelude.<$> initializationScripts,
            ("ec2SecurityGroupIds" Core..=)
              Prelude.<$> ec2SecurityGroupIds,
            ("subtype" Core..=) Prelude.<$> subtype,
            ("scriptParameters" Core..=)
              Prelude.<$> scriptParameters,
            ("configuration" Core..=) Prelude.<$> configuration,
            ("description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("type" Core..= type')
          ]
      )

instance Core.ToPath CreateStudioComponent where
  toPath CreateStudioComponent' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/studio-components"
      ]

instance Core.ToQuery CreateStudioComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStudioComponentResponse' smart constructor.
data CreateStudioComponentResponse = CreateStudioComponentResponse'
  { -- | Information about the studio component.
    studioComponent :: Prelude.Maybe StudioComponent,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStudioComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioComponent', 'createStudioComponentResponse_studioComponent' - Information about the studio component.
--
-- 'httpStatus', 'createStudioComponentResponse_httpStatus' - The response's http status code.
newCreateStudioComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStudioComponentResponse
newCreateStudioComponentResponse pHttpStatus_ =
  CreateStudioComponentResponse'
    { studioComponent =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the studio component.
createStudioComponentResponse_studioComponent :: Lens.Lens' CreateStudioComponentResponse (Prelude.Maybe StudioComponent)
createStudioComponentResponse_studioComponent = Lens.lens (\CreateStudioComponentResponse' {studioComponent} -> studioComponent) (\s@CreateStudioComponentResponse' {} a -> s {studioComponent = a} :: CreateStudioComponentResponse)

-- | The response's http status code.
createStudioComponentResponse_httpStatus :: Lens.Lens' CreateStudioComponentResponse Prelude.Int
createStudioComponentResponse_httpStatus = Lens.lens (\CreateStudioComponentResponse' {httpStatus} -> httpStatus) (\s@CreateStudioComponentResponse' {} a -> s {httpStatus = a} :: CreateStudioComponentResponse)

instance Prelude.NFData CreateStudioComponentResponse
