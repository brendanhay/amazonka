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
-- Module      : Amazonka.Panorama.CreateApplicationInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application instance and deploys it to a device.
module Amazonka.Panorama.CreateApplicationInstance
  ( -- * Creating a Request
    CreateApplicationInstance (..),
    newCreateApplicationInstance,

    -- * Request Lenses
    createApplicationInstance_applicationInstanceIdToReplace,
    createApplicationInstance_description,
    createApplicationInstance_manifestOverridesPayload,
    createApplicationInstance_name,
    createApplicationInstance_runtimeRoleArn,
    createApplicationInstance_tags,
    createApplicationInstance_defaultRuntimeContextDevice,
    createApplicationInstance_manifestPayload,

    -- * Destructuring the Response
    CreateApplicationInstanceResponse (..),
    newCreateApplicationInstanceResponse,

    -- * Response Lenses
    createApplicationInstanceResponse_httpStatus,
    createApplicationInstanceResponse_applicationInstanceId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateApplicationInstance' smart constructor.
data CreateApplicationInstance = CreateApplicationInstance'
  { -- | The ID of an application instance to replace with the new instance.
    applicationInstanceIdToReplace :: Prelude.Maybe Prelude.Text,
    -- | A description for the application instance.
    description :: Prelude.Maybe Prelude.Text,
    -- | Setting overrides for the application manifest.
    manifestOverridesPayload :: Prelude.Maybe ManifestOverridesPayload,
    -- | A name for the application instance.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a runtime role for the application instance.
    runtimeRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Tags for the application instance.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A device\'s ID.
    defaultRuntimeContextDevice :: Prelude.Text,
    -- | The application\'s manifest document.
    manifestPayload :: ManifestPayload
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplicationInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationInstanceIdToReplace', 'createApplicationInstance_applicationInstanceIdToReplace' - The ID of an application instance to replace with the new instance.
--
-- 'description', 'createApplicationInstance_description' - A description for the application instance.
--
-- 'manifestOverridesPayload', 'createApplicationInstance_manifestOverridesPayload' - Setting overrides for the application manifest.
--
-- 'name', 'createApplicationInstance_name' - A name for the application instance.
--
-- 'runtimeRoleArn', 'createApplicationInstance_runtimeRoleArn' - The ARN of a runtime role for the application instance.
--
-- 'tags', 'createApplicationInstance_tags' - Tags for the application instance.
--
-- 'defaultRuntimeContextDevice', 'createApplicationInstance_defaultRuntimeContextDevice' - A device\'s ID.
--
-- 'manifestPayload', 'createApplicationInstance_manifestPayload' - The application\'s manifest document.
newCreateApplicationInstance ::
  -- | 'defaultRuntimeContextDevice'
  Prelude.Text ->
  -- | 'manifestPayload'
  ManifestPayload ->
  CreateApplicationInstance
newCreateApplicationInstance
  pDefaultRuntimeContextDevice_
  pManifestPayload_ =
    CreateApplicationInstance'
      { applicationInstanceIdToReplace =
          Prelude.Nothing,
        description = Prelude.Nothing,
        manifestOverridesPayload = Prelude.Nothing,
        name = Prelude.Nothing,
        runtimeRoleArn = Prelude.Nothing,
        tags = Prelude.Nothing,
        defaultRuntimeContextDevice =
          pDefaultRuntimeContextDevice_,
        manifestPayload = pManifestPayload_
      }

-- | The ID of an application instance to replace with the new instance.
createApplicationInstance_applicationInstanceIdToReplace :: Lens.Lens' CreateApplicationInstance (Prelude.Maybe Prelude.Text)
createApplicationInstance_applicationInstanceIdToReplace = Lens.lens (\CreateApplicationInstance' {applicationInstanceIdToReplace} -> applicationInstanceIdToReplace) (\s@CreateApplicationInstance' {} a -> s {applicationInstanceIdToReplace = a} :: CreateApplicationInstance)

-- | A description for the application instance.
createApplicationInstance_description :: Lens.Lens' CreateApplicationInstance (Prelude.Maybe Prelude.Text)
createApplicationInstance_description = Lens.lens (\CreateApplicationInstance' {description} -> description) (\s@CreateApplicationInstance' {} a -> s {description = a} :: CreateApplicationInstance)

-- | Setting overrides for the application manifest.
createApplicationInstance_manifestOverridesPayload :: Lens.Lens' CreateApplicationInstance (Prelude.Maybe ManifestOverridesPayload)
createApplicationInstance_manifestOverridesPayload = Lens.lens (\CreateApplicationInstance' {manifestOverridesPayload} -> manifestOverridesPayload) (\s@CreateApplicationInstance' {} a -> s {manifestOverridesPayload = a} :: CreateApplicationInstance)

-- | A name for the application instance.
createApplicationInstance_name :: Lens.Lens' CreateApplicationInstance (Prelude.Maybe Prelude.Text)
createApplicationInstance_name = Lens.lens (\CreateApplicationInstance' {name} -> name) (\s@CreateApplicationInstance' {} a -> s {name = a} :: CreateApplicationInstance)

-- | The ARN of a runtime role for the application instance.
createApplicationInstance_runtimeRoleArn :: Lens.Lens' CreateApplicationInstance (Prelude.Maybe Prelude.Text)
createApplicationInstance_runtimeRoleArn = Lens.lens (\CreateApplicationInstance' {runtimeRoleArn} -> runtimeRoleArn) (\s@CreateApplicationInstance' {} a -> s {runtimeRoleArn = a} :: CreateApplicationInstance)

-- | Tags for the application instance.
createApplicationInstance_tags :: Lens.Lens' CreateApplicationInstance (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApplicationInstance_tags = Lens.lens (\CreateApplicationInstance' {tags} -> tags) (\s@CreateApplicationInstance' {} a -> s {tags = a} :: CreateApplicationInstance) Prelude.. Lens.mapping Lens.coerced

-- | A device\'s ID.
createApplicationInstance_defaultRuntimeContextDevice :: Lens.Lens' CreateApplicationInstance Prelude.Text
createApplicationInstance_defaultRuntimeContextDevice = Lens.lens (\CreateApplicationInstance' {defaultRuntimeContextDevice} -> defaultRuntimeContextDevice) (\s@CreateApplicationInstance' {} a -> s {defaultRuntimeContextDevice = a} :: CreateApplicationInstance)

-- | The application\'s manifest document.
createApplicationInstance_manifestPayload :: Lens.Lens' CreateApplicationInstance ManifestPayload
createApplicationInstance_manifestPayload = Lens.lens (\CreateApplicationInstance' {manifestPayload} -> manifestPayload) (\s@CreateApplicationInstance' {} a -> s {manifestPayload = a} :: CreateApplicationInstance)

instance Core.AWSRequest CreateApplicationInstance where
  type
    AWSResponse CreateApplicationInstance =
      CreateApplicationInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApplicationInstanceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ApplicationInstanceId")
      )

instance Prelude.Hashable CreateApplicationInstance where
  hashWithSalt _salt CreateApplicationInstance' {..} =
    _salt
      `Prelude.hashWithSalt` applicationInstanceIdToReplace
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` manifestOverridesPayload
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` runtimeRoleArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` defaultRuntimeContextDevice
      `Prelude.hashWithSalt` manifestPayload

instance Prelude.NFData CreateApplicationInstance where
  rnf CreateApplicationInstance' {..} =
    Prelude.rnf applicationInstanceIdToReplace `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf manifestOverridesPayload `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf runtimeRoleArn `Prelude.seq`
              Prelude.rnf tags `Prelude.seq`
                Prelude.rnf defaultRuntimeContextDevice `Prelude.seq`
                  Prelude.rnf manifestPayload

instance Data.ToHeaders CreateApplicationInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApplicationInstance where
  toJSON CreateApplicationInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApplicationInstanceIdToReplace" Data..=)
              Prelude.<$> applicationInstanceIdToReplace,
            ("Description" Data..=) Prelude.<$> description,
            ("ManifestOverridesPayload" Data..=)
              Prelude.<$> manifestOverridesPayload,
            ("Name" Data..=) Prelude.<$> name,
            ("RuntimeRoleArn" Data..=)
              Prelude.<$> runtimeRoleArn,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "DefaultRuntimeContextDevice"
                  Data..= defaultRuntimeContextDevice
              ),
            Prelude.Just
              ("ManifestPayload" Data..= manifestPayload)
          ]
      )

instance Data.ToPath CreateApplicationInstance where
  toPath = Prelude.const "/application-instances"

instance Data.ToQuery CreateApplicationInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateApplicationInstanceResponse' smart constructor.
data CreateApplicationInstanceResponse = CreateApplicationInstanceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The application instance\'s ID.
    applicationInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplicationInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createApplicationInstanceResponse_httpStatus' - The response's http status code.
--
-- 'applicationInstanceId', 'createApplicationInstanceResponse_applicationInstanceId' - The application instance\'s ID.
newCreateApplicationInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationInstanceId'
  Prelude.Text ->
  CreateApplicationInstanceResponse
newCreateApplicationInstanceResponse
  pHttpStatus_
  pApplicationInstanceId_ =
    CreateApplicationInstanceResponse'
      { httpStatus =
          pHttpStatus_,
        applicationInstanceId =
          pApplicationInstanceId_
      }

-- | The response's http status code.
createApplicationInstanceResponse_httpStatus :: Lens.Lens' CreateApplicationInstanceResponse Prelude.Int
createApplicationInstanceResponse_httpStatus = Lens.lens (\CreateApplicationInstanceResponse' {httpStatus} -> httpStatus) (\s@CreateApplicationInstanceResponse' {} a -> s {httpStatus = a} :: CreateApplicationInstanceResponse)

-- | The application instance\'s ID.
createApplicationInstanceResponse_applicationInstanceId :: Lens.Lens' CreateApplicationInstanceResponse Prelude.Text
createApplicationInstanceResponse_applicationInstanceId = Lens.lens (\CreateApplicationInstanceResponse' {applicationInstanceId} -> applicationInstanceId) (\s@CreateApplicationInstanceResponse' {} a -> s {applicationInstanceId = a} :: CreateApplicationInstanceResponse)

instance
  Prelude.NFData
    CreateApplicationInstanceResponse
  where
  rnf CreateApplicationInstanceResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf applicationInstanceId
