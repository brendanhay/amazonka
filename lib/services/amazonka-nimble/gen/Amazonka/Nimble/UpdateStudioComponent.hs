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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a studio component resource.
module Amazonka.Nimble.UpdateStudioComponent
  ( -- * Creating a Request
    UpdateStudioComponent (..),
    newUpdateStudioComponent,

    -- * Request Lenses
    updateStudioComponent_scriptParameters,
    updateStudioComponent_name,
    updateStudioComponent_clientToken,
    updateStudioComponent_type,
    updateStudioComponent_initializationScripts,
    updateStudioComponent_configuration,
    updateStudioComponent_description,
    updateStudioComponent_subtype,
    updateStudioComponent_ec2SecurityGroupIds,
    updateStudioComponent_studioId,
    updateStudioComponent_studioComponentId,

    -- * Destructuring the Response
    UpdateStudioComponentResponse (..),
    newUpdateStudioComponentResponse,

    -- * Response Lenses
    updateStudioComponentResponse_studioComponent,
    updateStudioComponentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The studio component ID.
--
-- /See:/ 'newUpdateStudioComponent' smart constructor.
data UpdateStudioComponent = UpdateStudioComponent'
  { -- | Parameters for the studio component scripts.
    scriptParameters :: Prelude.Maybe [ScriptParameterKeyValue],
    -- | The name for the studio component.
    name :: Prelude.Maybe Prelude.Text,
    -- | To make an idempotent API request using one of these actions, specify a
    -- client token in the request. You should not reuse the same client token
    -- for other API requests. If you retry a request that completed
    -- successfully using the same client token and the same parameters, the
    -- retry succeeds without performing any further actions. If you retry a
    -- successful request using the same client token, but one or more of the
    -- parameters are different, the retry fails with a ValidationException
    -- error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The type of the studio component.
    type' :: Prelude.Maybe StudioComponentType,
    -- | Initialization scripts for studio components.
    initializationScripts :: Prelude.Maybe [StudioComponentInitializationScript],
    -- | The configuration of the studio component, based on component type.
    configuration :: Prelude.Maybe StudioComponentConfiguration,
    -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The specific subtype of a studio component.
    subtype :: Prelude.Maybe StudioComponentSubtype,
    -- | The EC2 security groups that control access to the studio component.
    ec2SecurityGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The studio ID.
    studioId :: Prelude.Text,
    -- | The studio component ID.
    studioComponentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStudioComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scriptParameters', 'updateStudioComponent_scriptParameters' - Parameters for the studio component scripts.
--
-- 'name', 'updateStudioComponent_name' - The name for the studio component.
--
-- 'clientToken', 'updateStudioComponent_clientToken' - To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
--
-- 'type'', 'updateStudioComponent_type' - The type of the studio component.
--
-- 'initializationScripts', 'updateStudioComponent_initializationScripts' - Initialization scripts for studio components.
--
-- 'configuration', 'updateStudioComponent_configuration' - The configuration of the studio component, based on component type.
--
-- 'description', 'updateStudioComponent_description' - The description.
--
-- 'subtype', 'updateStudioComponent_subtype' - The specific subtype of a studio component.
--
-- 'ec2SecurityGroupIds', 'updateStudioComponent_ec2SecurityGroupIds' - The EC2 security groups that control access to the studio component.
--
-- 'studioId', 'updateStudioComponent_studioId' - The studio ID.
--
-- 'studioComponentId', 'updateStudioComponent_studioComponentId' - The studio component ID.
newUpdateStudioComponent ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'studioComponentId'
  Prelude.Text ->
  UpdateStudioComponent
newUpdateStudioComponent
  pStudioId_
  pStudioComponentId_ =
    UpdateStudioComponent'
      { scriptParameters =
          Prelude.Nothing,
        name = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        type' = Prelude.Nothing,
        initializationScripts = Prelude.Nothing,
        configuration = Prelude.Nothing,
        description = Prelude.Nothing,
        subtype = Prelude.Nothing,
        ec2SecurityGroupIds = Prelude.Nothing,
        studioId = pStudioId_,
        studioComponentId = pStudioComponentId_
      }

-- | Parameters for the studio component scripts.
updateStudioComponent_scriptParameters :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe [ScriptParameterKeyValue])
updateStudioComponent_scriptParameters = Lens.lens (\UpdateStudioComponent' {scriptParameters} -> scriptParameters) (\s@UpdateStudioComponent' {} a -> s {scriptParameters = a} :: UpdateStudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | The name for the studio component.
updateStudioComponent_name :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe Prelude.Text)
updateStudioComponent_name = Lens.lens (\UpdateStudioComponent' {name} -> name) (\s@UpdateStudioComponent' {} a -> s {name = a} :: UpdateStudioComponent)

-- | To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
updateStudioComponent_clientToken :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe Prelude.Text)
updateStudioComponent_clientToken = Lens.lens (\UpdateStudioComponent' {clientToken} -> clientToken) (\s@UpdateStudioComponent' {} a -> s {clientToken = a} :: UpdateStudioComponent)

-- | The type of the studio component.
updateStudioComponent_type :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe StudioComponentType)
updateStudioComponent_type = Lens.lens (\UpdateStudioComponent' {type'} -> type') (\s@UpdateStudioComponent' {} a -> s {type' = a} :: UpdateStudioComponent)

-- | Initialization scripts for studio components.
updateStudioComponent_initializationScripts :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe [StudioComponentInitializationScript])
updateStudioComponent_initializationScripts = Lens.lens (\UpdateStudioComponent' {initializationScripts} -> initializationScripts) (\s@UpdateStudioComponent' {} a -> s {initializationScripts = a} :: UpdateStudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | The configuration of the studio component, based on component type.
updateStudioComponent_configuration :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe StudioComponentConfiguration)
updateStudioComponent_configuration = Lens.lens (\UpdateStudioComponent' {configuration} -> configuration) (\s@UpdateStudioComponent' {} a -> s {configuration = a} :: UpdateStudioComponent)

-- | The description.
updateStudioComponent_description :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe Prelude.Text)
updateStudioComponent_description = Lens.lens (\UpdateStudioComponent' {description} -> description) (\s@UpdateStudioComponent' {} a -> s {description = a} :: UpdateStudioComponent)

-- | The specific subtype of a studio component.
updateStudioComponent_subtype :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe StudioComponentSubtype)
updateStudioComponent_subtype = Lens.lens (\UpdateStudioComponent' {subtype} -> subtype) (\s@UpdateStudioComponent' {} a -> s {subtype = a} :: UpdateStudioComponent)

-- | The EC2 security groups that control access to the studio component.
updateStudioComponent_ec2SecurityGroupIds :: Lens.Lens' UpdateStudioComponent (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateStudioComponent_ec2SecurityGroupIds = Lens.lens (\UpdateStudioComponent' {ec2SecurityGroupIds} -> ec2SecurityGroupIds) (\s@UpdateStudioComponent' {} a -> s {ec2SecurityGroupIds = a} :: UpdateStudioComponent) Prelude.. Lens.mapping Lens.coerced

-- | The studio ID.
updateStudioComponent_studioId :: Lens.Lens' UpdateStudioComponent Prelude.Text
updateStudioComponent_studioId = Lens.lens (\UpdateStudioComponent' {studioId} -> studioId) (\s@UpdateStudioComponent' {} a -> s {studioId = a} :: UpdateStudioComponent)

-- | The studio component ID.
updateStudioComponent_studioComponentId :: Lens.Lens' UpdateStudioComponent Prelude.Text
updateStudioComponent_studioComponentId = Lens.lens (\UpdateStudioComponent' {studioComponentId} -> studioComponentId) (\s@UpdateStudioComponent' {} a -> s {studioComponentId = a} :: UpdateStudioComponent)

instance Core.AWSRequest UpdateStudioComponent where
  type
    AWSResponse UpdateStudioComponent =
      UpdateStudioComponentResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStudioComponentResponse'
            Prelude.<$> (x Core..?> "studioComponent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStudioComponent where
  hashWithSalt _salt UpdateStudioComponent' {..} =
    _salt `Prelude.hashWithSalt` scriptParameters
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` initializationScripts
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` subtype
      `Prelude.hashWithSalt` ec2SecurityGroupIds
      `Prelude.hashWithSalt` studioId
      `Prelude.hashWithSalt` studioComponentId

instance Prelude.NFData UpdateStudioComponent where
  rnf UpdateStudioComponent' {..} =
    Prelude.rnf scriptParameters
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf initializationScripts
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf subtype
      `Prelude.seq` Prelude.rnf ec2SecurityGroupIds
      `Prelude.seq` Prelude.rnf studioId
      `Prelude.seq` Prelude.rnf studioComponentId

instance Core.ToHeaders UpdateStudioComponent where
  toHeaders UpdateStudioComponent' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Core.=# clientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON UpdateStudioComponent where
  toJSON UpdateStudioComponent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("scriptParameters" Core..=)
              Prelude.<$> scriptParameters,
            ("name" Core..=) Prelude.<$> name,
            ("type" Core..=) Prelude.<$> type',
            ("initializationScripts" Core..=)
              Prelude.<$> initializationScripts,
            ("configuration" Core..=) Prelude.<$> configuration,
            ("description" Core..=) Prelude.<$> description,
            ("subtype" Core..=) Prelude.<$> subtype,
            ("ec2SecurityGroupIds" Core..=)
              Prelude.<$> ec2SecurityGroupIds
          ]
      )

instance Core.ToPath UpdateStudioComponent where
  toPath UpdateStudioComponent' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/studio-components/",
        Core.toBS studioComponentId
      ]

instance Core.ToQuery UpdateStudioComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStudioComponentResponse' smart constructor.
data UpdateStudioComponentResponse = UpdateStudioComponentResponse'
  { -- | Information about the studio component.
    studioComponent :: Prelude.Maybe StudioComponent,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
