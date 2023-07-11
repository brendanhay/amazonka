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
-- Module      : Amazonka.Nimble.UpdateStudioComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a studio component resource.
module Amazonka.Nimble.UpdateStudioComponent
  ( -- * Creating a Request
    UpdateStudioComponent (..),
    newUpdateStudioComponent,

    -- * Request Lenses
    updateStudioComponent_clientToken,
    updateStudioComponent_configuration,
    updateStudioComponent_description,
    updateStudioComponent_ec2SecurityGroupIds,
    updateStudioComponent_initializationScripts,
    updateStudioComponent_name,
    updateStudioComponent_runtimeRoleArn,
    updateStudioComponent_scriptParameters,
    updateStudioComponent_secureInitializationRoleArn,
    updateStudioComponent_subtype,
    updateStudioComponent_type,
    updateStudioComponent_studioComponentId,
    updateStudioComponent_studioId,

    -- * Destructuring the Response
    UpdateStudioComponentResponse (..),
    newUpdateStudioComponentResponse,

    -- * Response Lenses
    updateStudioComponentResponse_studioComponent,
    updateStudioComponentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateStudioComponent' smart constructor.
data UpdateStudioComponent = UpdateStudioComponent'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the
    -- Amazon Web Services SDK automatically generates a client token and uses
    -- it for the request to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The configuration of the studio component, based on component type.
    configuration :: Prelude.Maybe StudioComponentConfiguration,
    -- | The description.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The EC2 security groups that control access to the studio component.
    ec2SecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | Initialization scripts for studio components.
    initializationScripts :: Prelude.Maybe [StudioComponentInitializationScript],
    -- | The name for the studio component.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | An IAM role attached to a Studio Component that gives the studio
    -- component access to Amazon Web Services resources at anytime while the
    -- instance is running.
    runtimeRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Parameters for the studio component scripts.
    scriptParameters :: Prelude.Maybe (Data.Sensitive [ScriptParameterKeyValue]),
    -- | An IAM role attached to Studio Component when the system initialization
    -- script runs which give the studio component access to Amazon Web
    -- Services resources when the system initialization script runs.
    secureInitializationRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The specific subtype of a studio component.
    subtype :: Prelude.Maybe StudioComponentSubtype,
    -- | The type of the studio component.
    type' :: Prelude.Maybe StudioComponentType,
    -- | The studio component ID.
    studioComponentId :: Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStudioComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateStudioComponent_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the
-- Amazon Web Services SDK automatically generates a client token and uses
-- it for the request to ensure idempotency.
--
-- 'configuration', 'updateStudioComponent_configuration' - The configuration of the studio component, based on component type.
--
-- 'description', 'updateStudioComponent_description' - The description.
--
-- 'ec2SecurityGroupIds', 'updateStudioComponent_ec2SecurityGroupIds' - The EC2 security groups that control access to the studio component.
--
-- 'initializationScripts', 'updateStudioComponent_initializationScripts' - Initialization scripts for studio components.
--
-- 'name', 'updateStudioComponent_name' - The name for the studio component.
--
-- 'runtimeRoleArn', 'updateStudioComponent_runtimeRoleArn' - An IAM role attached to a Studio Component that gives the studio
-- component access to Amazon Web Services resources at anytime while the
-- instance is running.
--
-- 'scriptParameters', 'updateStudioComponent_scriptParameters' - Parameters for the studio component scripts.
--
-- 'secureInitializationRoleArn', 'updateStudioComponent_secureInitializationRoleArn' - An IAM role attached to Studio Component when the system initialization
-- script runs which give the studio component access to Amazon Web
-- Services resources when the system initialization script runs.
--
-- 'subtype', 'updateStudioComponent_subtype' - The specific subtype of a studio component.
--
-- 'type'', 'updateStudioComponent_type' - The type of the studio component.
--
-- 'studioComponentId', 'updateStudioComponent_studioComponentId' - The studio component ID.
--
-- 'studioId', 'updateStudioComponent_studioId' - The studio ID.
newUpdateStudioComponent ::
  -- | 'studioComponentId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  UpdateStudioComponent
newUpdateStudioComponent
  pStudioComponentId_
  pStudioId_ =
    UpdateStudioComponent'
      { clientToken =
          Prelude.Nothing,
        configuration = Prelude.Nothing,
        description = Prelude.Nothing,
        ec2SecurityGroupIds = Prelude.Nothing,
        initializationScripts = Prelude.Nothing,
        name = Prelude.Nothing,
        runtimeRoleArn = Prelude.Nothing,
        scriptParameters = Prelude.Nothing,
        secureInitializationRoleArn = Prelude.Nothing,
        subtype = Prelude.Nothing,
        type' = Prelude.Nothing,
        studioComponentId = pStudioComponentId_,
        studioId = pStudioId_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the
-- Amazon Web Services SDK automatically generates a client token and uses
-- it for the request to ensure idempotency.
updateStudioComponent_clientToken :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe Prelude.Text)
updateStudioComponent_clientToken = Lens.lens (\UpdateStudioComponent' {clientToken} -> clientToken) (\s@UpdateStudioComponent' {} a -> s {clientToken = a} :: UpdateStudioComponent)

-- | The configuration of the studio component, based on component type.
updateStudioComponent_configuration :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe StudioComponentConfiguration)
updateStudioComponent_configuration = Lens.lens (\UpdateStudioComponent' {configuration} -> configuration) (\s@UpdateStudioComponent' {} a -> s {configuration = a} :: UpdateStudioComponent)

-- | The description.
updateStudioComponent_description :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe Prelude.Text)
updateStudioComponent_description = Lens.lens (\UpdateStudioComponent' {description} -> description) (\s@UpdateStudioComponent' {} a -> s {description = a} :: UpdateStudioComponent) Prelude.. Lens.mapping Data._Sensitive

-- | The EC2 security groups that control access to the studio component.
updateStudioComponent_ec2SecurityGroupIds :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe [Prelude.Text])
updateStudioComponent_ec2SecurityGroupIds = Lens.lens (\UpdateStudioComponent' {ec2SecurityGroupIds} -> ec2SecurityGroupIds) (\s@UpdateStudioComponent' {} a -> s {ec2SecurityGroupIds = a} :: UpdateStudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | Initialization scripts for studio components.
updateStudioComponent_initializationScripts :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe [StudioComponentInitializationScript])
updateStudioComponent_initializationScripts = Lens.lens (\UpdateStudioComponent' {initializationScripts} -> initializationScripts) (\s@UpdateStudioComponent' {} a -> s {initializationScripts = a} :: UpdateStudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | The name for the studio component.
updateStudioComponent_name :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe Prelude.Text)
updateStudioComponent_name = Lens.lens (\UpdateStudioComponent' {name} -> name) (\s@UpdateStudioComponent' {} a -> s {name = a} :: UpdateStudioComponent) Prelude.. Lens.mapping Data._Sensitive

-- | An IAM role attached to a Studio Component that gives the studio
-- component access to Amazon Web Services resources at anytime while the
-- instance is running.
updateStudioComponent_runtimeRoleArn :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe Prelude.Text)
updateStudioComponent_runtimeRoleArn = Lens.lens (\UpdateStudioComponent' {runtimeRoleArn} -> runtimeRoleArn) (\s@UpdateStudioComponent' {} a -> s {runtimeRoleArn = a} :: UpdateStudioComponent)

-- | Parameters for the studio component scripts.
updateStudioComponent_scriptParameters :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe [ScriptParameterKeyValue])
updateStudioComponent_scriptParameters = Lens.lens (\UpdateStudioComponent' {scriptParameters} -> scriptParameters) (\s@UpdateStudioComponent' {} a -> s {scriptParameters = a} :: UpdateStudioComponent) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | An IAM role attached to Studio Component when the system initialization
-- script runs which give the studio component access to Amazon Web
-- Services resources when the system initialization script runs.
updateStudioComponent_secureInitializationRoleArn :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe Prelude.Text)
updateStudioComponent_secureInitializationRoleArn = Lens.lens (\UpdateStudioComponent' {secureInitializationRoleArn} -> secureInitializationRoleArn) (\s@UpdateStudioComponent' {} a -> s {secureInitializationRoleArn = a} :: UpdateStudioComponent)

-- | The specific subtype of a studio component.
updateStudioComponent_subtype :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe StudioComponentSubtype)
updateStudioComponent_subtype = Lens.lens (\UpdateStudioComponent' {subtype} -> subtype) (\s@UpdateStudioComponent' {} a -> s {subtype = a} :: UpdateStudioComponent)

-- | The type of the studio component.
updateStudioComponent_type :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe StudioComponentType)
updateStudioComponent_type = Lens.lens (\UpdateStudioComponent' {type'} -> type') (\s@UpdateStudioComponent' {} a -> s {type' = a} :: UpdateStudioComponent)

-- | The studio component ID.
updateStudioComponent_studioComponentId :: Lens.Lens' UpdateStudioComponent Prelude.Text
updateStudioComponent_studioComponentId = Lens.lens (\UpdateStudioComponent' {studioComponentId} -> studioComponentId) (\s@UpdateStudioComponent' {} a -> s {studioComponentId = a} :: UpdateStudioComponent)

-- | The studio ID.
updateStudioComponent_studioId :: Lens.Lens' UpdateStudioComponent Prelude.Text
updateStudioComponent_studioId = Lens.lens (\UpdateStudioComponent' {studioId} -> studioId) (\s@UpdateStudioComponent' {} a -> s {studioId = a} :: UpdateStudioComponent)

instance Core.AWSRequest UpdateStudioComponent where
  type
    AWSResponse UpdateStudioComponent =
      UpdateStudioComponentResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStudioComponentResponse'
            Prelude.<$> (x Data..?> "studioComponent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStudioComponent where
  hashWithSalt _salt UpdateStudioComponent' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` ec2SecurityGroupIds
      `Prelude.hashWithSalt` initializationScripts
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` runtimeRoleArn
      `Prelude.hashWithSalt` scriptParameters
      `Prelude.hashWithSalt` secureInitializationRoleArn
      `Prelude.hashWithSalt` subtype
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` studioComponentId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData UpdateStudioComponent where
  rnf UpdateStudioComponent' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf ec2SecurityGroupIds
      `Prelude.seq` Prelude.rnf initializationScripts
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf runtimeRoleArn
      `Prelude.seq` Prelude.rnf scriptParameters
      `Prelude.seq` Prelude.rnf secureInitializationRoleArn
      `Prelude.seq` Prelude.rnf subtype
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf studioComponentId
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders UpdateStudioComponent where
  toHeaders UpdateStudioComponent' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON UpdateStudioComponent where
  toJSON UpdateStudioComponent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("configuration" Data..=) Prelude.<$> configuration,
            ("description" Data..=) Prelude.<$> description,
            ("ec2SecurityGroupIds" Data..=)
              Prelude.<$> ec2SecurityGroupIds,
            ("initializationScripts" Data..=)
              Prelude.<$> initializationScripts,
            ("name" Data..=) Prelude.<$> name,
            ("runtimeRoleArn" Data..=)
              Prelude.<$> runtimeRoleArn,
            ("scriptParameters" Data..=)
              Prelude.<$> scriptParameters,
            ("secureInitializationRoleArn" Data..=)
              Prelude.<$> secureInitializationRoleArn,
            ("subtype" Data..=) Prelude.<$> subtype,
            ("type" Data..=) Prelude.<$> type'
          ]
      )

instance Data.ToPath UpdateStudioComponent where
  toPath UpdateStudioComponent' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/studio-components/",
        Data.toBS studioComponentId
      ]

instance Data.ToQuery UpdateStudioComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStudioComponentResponse' smart constructor.
data UpdateStudioComponentResponse = UpdateStudioComponentResponse'
  { -- | Information about the studio component.
    studioComponent :: Prelude.Maybe StudioComponent,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStudioComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioComponent', 'updateStudioComponentResponse_studioComponent' - Information about the studio component.
--
-- 'httpStatus', 'updateStudioComponentResponse_httpStatus' - The response's http status code.
newUpdateStudioComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateStudioComponentResponse
newUpdateStudioComponentResponse pHttpStatus_ =
  UpdateStudioComponentResponse'
    { studioComponent =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the studio component.
updateStudioComponentResponse_studioComponent :: Lens.Lens' UpdateStudioComponentResponse (Prelude.Maybe StudioComponent)
updateStudioComponentResponse_studioComponent = Lens.lens (\UpdateStudioComponentResponse' {studioComponent} -> studioComponent) (\s@UpdateStudioComponentResponse' {} a -> s {studioComponent = a} :: UpdateStudioComponentResponse)

-- | The response's http status code.
updateStudioComponentResponse_httpStatus :: Lens.Lens' UpdateStudioComponentResponse Prelude.Int
updateStudioComponentResponse_httpStatus = Lens.lens (\UpdateStudioComponentResponse' {httpStatus} -> httpStatus) (\s@UpdateStudioComponentResponse' {} a -> s {httpStatus = a} :: UpdateStudioComponentResponse)

instance Prelude.NFData UpdateStudioComponentResponse where
  rnf UpdateStudioComponentResponse' {..} =
    Prelude.rnf studioComponent
      `Prelude.seq` Prelude.rnf httpStatus
