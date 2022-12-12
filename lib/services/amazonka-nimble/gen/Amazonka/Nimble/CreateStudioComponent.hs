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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    createStudioComponent_clientToken,
    createStudioComponent_configuration,
    createStudioComponent_description,
    createStudioComponent_ec2SecurityGroupIds,
    createStudioComponent_initializationScripts,
    createStudioComponent_runtimeRoleArn,
    createStudioComponent_scriptParameters,
    createStudioComponent_secureInitializationRoleArn,
    createStudioComponent_subtype,
    createStudioComponent_tags,
    createStudioComponent_name,
    createStudioComponent_studioId,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateStudioComponent' smart constructor.
data CreateStudioComponent = CreateStudioComponent'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the AWS
    -- SDK automatically generates a client token and uses it for the request
    -- to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The configuration of the studio component, based on component type.
    configuration :: Prelude.Maybe StudioComponentConfiguration,
    -- | The description.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The EC2 security groups that control access to the studio component.
    ec2SecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | Initialization scripts for studio components.
    initializationScripts :: Prelude.Maybe [StudioComponentInitializationScript],
    -- | An IAM role attached to a Studio Component that gives the studio
    -- component access to AWS resources at anytime while the instance is
    -- running.
    runtimeRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Parameters for the studio component scripts.
    scriptParameters :: Prelude.Maybe (Data.Sensitive [ScriptParameterKeyValue]),
    -- | An IAM role attached to Studio Component when the system initialization
    -- script runs which give the studio component access to AWS resources when
    -- the system initialization script runs.
    secureInitializationRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The specific subtype of a studio component.
    subtype :: Prelude.Maybe StudioComponentSubtype,
    -- | A collection of labels, in the form of key:value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name for the studio component.
    name :: Data.Sensitive Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text,
    -- | The type of the studio component.
    type' :: StudioComponentType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStudioComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createStudioComponent_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
--
-- 'configuration', 'createStudioComponent_configuration' - The configuration of the studio component, based on component type.
--
-- 'description', 'createStudioComponent_description' - The description.
--
-- 'ec2SecurityGroupIds', 'createStudioComponent_ec2SecurityGroupIds' - The EC2 security groups that control access to the studio component.
--
-- 'initializationScripts', 'createStudioComponent_initializationScripts' - Initialization scripts for studio components.
--
-- 'runtimeRoleArn', 'createStudioComponent_runtimeRoleArn' - An IAM role attached to a Studio Component that gives the studio
-- component access to AWS resources at anytime while the instance is
-- running.
--
-- 'scriptParameters', 'createStudioComponent_scriptParameters' - Parameters for the studio component scripts.
--
-- 'secureInitializationRoleArn', 'createStudioComponent_secureInitializationRoleArn' - An IAM role attached to Studio Component when the system initialization
-- script runs which give the studio component access to AWS resources when
-- the system initialization script runs.
--
-- 'subtype', 'createStudioComponent_subtype' - The specific subtype of a studio component.
--
-- 'tags', 'createStudioComponent_tags' - A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
--
-- 'name', 'createStudioComponent_name' - The name for the studio component.
--
-- 'studioId', 'createStudioComponent_studioId' - The studio ID.
--
-- 'type'', 'createStudioComponent_type' - The type of the studio component.
newCreateStudioComponent ::
  -- | 'name'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  -- | 'type''
  StudioComponentType ->
  CreateStudioComponent
newCreateStudioComponent pName_ pStudioId_ pType_ =
  CreateStudioComponent'
    { clientToken =
        Prelude.Nothing,
      configuration = Prelude.Nothing,
      description = Prelude.Nothing,
      ec2SecurityGroupIds = Prelude.Nothing,
      initializationScripts = Prelude.Nothing,
      runtimeRoleArn = Prelude.Nothing,
      scriptParameters = Prelude.Nothing,
      secureInitializationRoleArn = Prelude.Nothing,
      subtype = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = Data._Sensitive Lens.# pName_,
      studioId = pStudioId_,
      type' = pType_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
createStudioComponent_clientToken :: Lens.Lens' CreateStudioComponent (Prelude.Maybe Prelude.Text)
createStudioComponent_clientToken = Lens.lens (\CreateStudioComponent' {clientToken} -> clientToken) (\s@CreateStudioComponent' {} a -> s {clientToken = a} :: CreateStudioComponent)

-- | The configuration of the studio component, based on component type.
createStudioComponent_configuration :: Lens.Lens' CreateStudioComponent (Prelude.Maybe StudioComponentConfiguration)
createStudioComponent_configuration = Lens.lens (\CreateStudioComponent' {configuration} -> configuration) (\s@CreateStudioComponent' {} a -> s {configuration = a} :: CreateStudioComponent)

-- | The description.
createStudioComponent_description :: Lens.Lens' CreateStudioComponent (Prelude.Maybe Prelude.Text)
createStudioComponent_description = Lens.lens (\CreateStudioComponent' {description} -> description) (\s@CreateStudioComponent' {} a -> s {description = a} :: CreateStudioComponent) Prelude.. Lens.mapping Data._Sensitive

-- | The EC2 security groups that control access to the studio component.
createStudioComponent_ec2SecurityGroupIds :: Lens.Lens' CreateStudioComponent (Prelude.Maybe [Prelude.Text])
createStudioComponent_ec2SecurityGroupIds = Lens.lens (\CreateStudioComponent' {ec2SecurityGroupIds} -> ec2SecurityGroupIds) (\s@CreateStudioComponent' {} a -> s {ec2SecurityGroupIds = a} :: CreateStudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | Initialization scripts for studio components.
createStudioComponent_initializationScripts :: Lens.Lens' CreateStudioComponent (Prelude.Maybe [StudioComponentInitializationScript])
createStudioComponent_initializationScripts = Lens.lens (\CreateStudioComponent' {initializationScripts} -> initializationScripts) (\s@CreateStudioComponent' {} a -> s {initializationScripts = a} :: CreateStudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | An IAM role attached to a Studio Component that gives the studio
-- component access to AWS resources at anytime while the instance is
-- running.
createStudioComponent_runtimeRoleArn :: Lens.Lens' CreateStudioComponent (Prelude.Maybe Prelude.Text)
createStudioComponent_runtimeRoleArn = Lens.lens (\CreateStudioComponent' {runtimeRoleArn} -> runtimeRoleArn) (\s@CreateStudioComponent' {} a -> s {runtimeRoleArn = a} :: CreateStudioComponent)

-- | Parameters for the studio component scripts.
createStudioComponent_scriptParameters :: Lens.Lens' CreateStudioComponent (Prelude.Maybe [ScriptParameterKeyValue])
createStudioComponent_scriptParameters = Lens.lens (\CreateStudioComponent' {scriptParameters} -> scriptParameters) (\s@CreateStudioComponent' {} a -> s {scriptParameters = a} :: CreateStudioComponent) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | An IAM role attached to Studio Component when the system initialization
-- script runs which give the studio component access to AWS resources when
-- the system initialization script runs.
createStudioComponent_secureInitializationRoleArn :: Lens.Lens' CreateStudioComponent (Prelude.Maybe Prelude.Text)
createStudioComponent_secureInitializationRoleArn = Lens.lens (\CreateStudioComponent' {secureInitializationRoleArn} -> secureInitializationRoleArn) (\s@CreateStudioComponent' {} a -> s {secureInitializationRoleArn = a} :: CreateStudioComponent)

-- | The specific subtype of a studio component.
createStudioComponent_subtype :: Lens.Lens' CreateStudioComponent (Prelude.Maybe StudioComponentSubtype)
createStudioComponent_subtype = Lens.lens (\CreateStudioComponent' {subtype} -> subtype) (\s@CreateStudioComponent' {} a -> s {subtype = a} :: CreateStudioComponent)

-- | A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
createStudioComponent_tags :: Lens.Lens' CreateStudioComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStudioComponent_tags = Lens.lens (\CreateStudioComponent' {tags} -> tags) (\s@CreateStudioComponent' {} a -> s {tags = a} :: CreateStudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | The name for the studio component.
createStudioComponent_name :: Lens.Lens' CreateStudioComponent Prelude.Text
createStudioComponent_name = Lens.lens (\CreateStudioComponent' {name} -> name) (\s@CreateStudioComponent' {} a -> s {name = a} :: CreateStudioComponent) Prelude.. Data._Sensitive

-- | The studio ID.
createStudioComponent_studioId :: Lens.Lens' CreateStudioComponent Prelude.Text
createStudioComponent_studioId = Lens.lens (\CreateStudioComponent' {studioId} -> studioId) (\s@CreateStudioComponent' {} a -> s {studioId = a} :: CreateStudioComponent)

-- | The type of the studio component.
createStudioComponent_type :: Lens.Lens' CreateStudioComponent StudioComponentType
createStudioComponent_type = Lens.lens (\CreateStudioComponent' {type'} -> type') (\s@CreateStudioComponent' {} a -> s {type' = a} :: CreateStudioComponent)

instance Core.AWSRequest CreateStudioComponent where
  type
    AWSResponse CreateStudioComponent =
      CreateStudioComponentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStudioComponentResponse'
            Prelude.<$> (x Data..?> "studioComponent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStudioComponent where
  hashWithSalt _salt CreateStudioComponent' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` ec2SecurityGroupIds
      `Prelude.hashWithSalt` initializationScripts
      `Prelude.hashWithSalt` runtimeRoleArn
      `Prelude.hashWithSalt` scriptParameters
      `Prelude.hashWithSalt` secureInitializationRoleArn
      `Prelude.hashWithSalt` subtype
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` studioId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateStudioComponent where
  rnf CreateStudioComponent' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf ec2SecurityGroupIds
      `Prelude.seq` Prelude.rnf initializationScripts
      `Prelude.seq` Prelude.rnf runtimeRoleArn
      `Prelude.seq` Prelude.rnf scriptParameters
      `Prelude.seq` Prelude.rnf secureInitializationRoleArn
      `Prelude.seq` Prelude.rnf subtype
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf studioId
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders CreateStudioComponent where
  toHeaders CreateStudioComponent' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateStudioComponent where
  toJSON CreateStudioComponent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("configuration" Data..=) Prelude.<$> configuration,
            ("description" Data..=) Prelude.<$> description,
            ("ec2SecurityGroupIds" Data..=)
              Prelude.<$> ec2SecurityGroupIds,
            ("initializationScripts" Data..=)
              Prelude.<$> initializationScripts,
            ("runtimeRoleArn" Data..=)
              Prelude.<$> runtimeRoleArn,
            ("scriptParameters" Data..=)
              Prelude.<$> scriptParameters,
            ("secureInitializationRoleArn" Data..=)
              Prelude.<$> secureInitializationRoleArn,
            ("subtype" Data..=) Prelude.<$> subtype,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath CreateStudioComponent where
  toPath CreateStudioComponent' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/studio-components"
      ]

instance Data.ToQuery CreateStudioComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStudioComponentResponse' smart constructor.
data CreateStudioComponentResponse = CreateStudioComponentResponse'
  { -- | Information about the studio component.
    studioComponent :: Prelude.Maybe StudioComponent,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateStudioComponentResponse where
  rnf CreateStudioComponentResponse' {..} =
    Prelude.rnf studioComponent
      `Prelude.seq` Prelude.rnf httpStatus
