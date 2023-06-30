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
-- Module      : Amazonka.AppRunner.CreateObservabilityConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an App Runner observability configuration resource. App Runner
-- requires this resource when you create or update App Runner services and
-- you want to enable non-default observability features. You can share an
-- observability configuration across multiple services.
--
-- Create multiple revisions of a configuration by calling this action
-- multiple times using the same @ObservabilityConfigurationName@. The call
-- returns incremental @ObservabilityConfigurationRevision@ values. When
-- you create a service and configure an observability configuration
-- resource, the service uses the latest active revision of the
-- observability configuration by default. You can optionally configure the
-- service to use a specific revision.
--
-- The observability configuration resource is designed to configure
-- multiple features (currently one feature, tracing). This action takes
-- optional parameters that describe the configuration of these features
-- (currently one parameter, @TraceConfiguration@). If you don\'t specify a
-- feature parameter, App Runner doesn\'t enable the feature.
module Amazonka.AppRunner.CreateObservabilityConfiguration
  ( -- * Creating a Request
    CreateObservabilityConfiguration (..),
    newCreateObservabilityConfiguration,

    -- * Request Lenses
    createObservabilityConfiguration_tags,
    createObservabilityConfiguration_traceConfiguration,
    createObservabilityConfiguration_observabilityConfigurationName,

    -- * Destructuring the Response
    CreateObservabilityConfigurationResponse (..),
    newCreateObservabilityConfigurationResponse,

    -- * Response Lenses
    createObservabilityConfigurationResponse_httpStatus,
    createObservabilityConfigurationResponse_observabilityConfiguration,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateObservabilityConfiguration' smart constructor.
data CreateObservabilityConfiguration = CreateObservabilityConfiguration'
  { -- | A list of metadata items that you can associate with your observability
    -- configuration resource. A tag is a key-value pair.
    tags :: Prelude.Maybe [Tag],
    -- | The configuration of the tracing feature within this observability
    -- configuration. If you don\'t specify it, App Runner doesn\'t enable
    -- tracing.
    traceConfiguration :: Prelude.Maybe TraceConfiguration,
    -- | A name for the observability configuration. When you use it for the
    -- first time in an Amazon Web Services Region, App Runner creates revision
    -- number @1@ of this name. When you use the same name in subsequent calls,
    -- App Runner creates incremental revisions of the configuration.
    --
    -- The name @DefaultConfiguration@ is reserved. You can\'t use it to create
    -- a new observability configuration, and you can\'t create a revision of
    -- it.
    --
    -- When you want to use your own observability configuration for your App
    -- Runner service, /create a configuration with a different name/, and then
    -- provide it when you create or update your service.
    observabilityConfigurationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateObservabilityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createObservabilityConfiguration_tags' - A list of metadata items that you can associate with your observability
-- configuration resource. A tag is a key-value pair.
--
-- 'traceConfiguration', 'createObservabilityConfiguration_traceConfiguration' - The configuration of the tracing feature within this observability
-- configuration. If you don\'t specify it, App Runner doesn\'t enable
-- tracing.
--
-- 'observabilityConfigurationName', 'createObservabilityConfiguration_observabilityConfigurationName' - A name for the observability configuration. When you use it for the
-- first time in an Amazon Web Services Region, App Runner creates revision
-- number @1@ of this name. When you use the same name in subsequent calls,
-- App Runner creates incremental revisions of the configuration.
--
-- The name @DefaultConfiguration@ is reserved. You can\'t use it to create
-- a new observability configuration, and you can\'t create a revision of
-- it.
--
-- When you want to use your own observability configuration for your App
-- Runner service, /create a configuration with a different name/, and then
-- provide it when you create or update your service.
newCreateObservabilityConfiguration ::
  -- | 'observabilityConfigurationName'
  Prelude.Text ->
  CreateObservabilityConfiguration
newCreateObservabilityConfiguration
  pObservabilityConfigurationName_ =
    CreateObservabilityConfiguration'
      { tags =
          Prelude.Nothing,
        traceConfiguration = Prelude.Nothing,
        observabilityConfigurationName =
          pObservabilityConfigurationName_
      }

-- | A list of metadata items that you can associate with your observability
-- configuration resource. A tag is a key-value pair.
createObservabilityConfiguration_tags :: Lens.Lens' CreateObservabilityConfiguration (Prelude.Maybe [Tag])
createObservabilityConfiguration_tags = Lens.lens (\CreateObservabilityConfiguration' {tags} -> tags) (\s@CreateObservabilityConfiguration' {} a -> s {tags = a} :: CreateObservabilityConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The configuration of the tracing feature within this observability
-- configuration. If you don\'t specify it, App Runner doesn\'t enable
-- tracing.
createObservabilityConfiguration_traceConfiguration :: Lens.Lens' CreateObservabilityConfiguration (Prelude.Maybe TraceConfiguration)
createObservabilityConfiguration_traceConfiguration = Lens.lens (\CreateObservabilityConfiguration' {traceConfiguration} -> traceConfiguration) (\s@CreateObservabilityConfiguration' {} a -> s {traceConfiguration = a} :: CreateObservabilityConfiguration)

-- | A name for the observability configuration. When you use it for the
-- first time in an Amazon Web Services Region, App Runner creates revision
-- number @1@ of this name. When you use the same name in subsequent calls,
-- App Runner creates incremental revisions of the configuration.
--
-- The name @DefaultConfiguration@ is reserved. You can\'t use it to create
-- a new observability configuration, and you can\'t create a revision of
-- it.
--
-- When you want to use your own observability configuration for your App
-- Runner service, /create a configuration with a different name/, and then
-- provide it when you create or update your service.
createObservabilityConfiguration_observabilityConfigurationName :: Lens.Lens' CreateObservabilityConfiguration Prelude.Text
createObservabilityConfiguration_observabilityConfigurationName = Lens.lens (\CreateObservabilityConfiguration' {observabilityConfigurationName} -> observabilityConfigurationName) (\s@CreateObservabilityConfiguration' {} a -> s {observabilityConfigurationName = a} :: CreateObservabilityConfiguration)

instance
  Core.AWSRequest
    CreateObservabilityConfiguration
  where
  type
    AWSResponse CreateObservabilityConfiguration =
      CreateObservabilityConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateObservabilityConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ObservabilityConfiguration")
      )

instance
  Prelude.Hashable
    CreateObservabilityConfiguration
  where
  hashWithSalt
    _salt
    CreateObservabilityConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` traceConfiguration
        `Prelude.hashWithSalt` observabilityConfigurationName

instance
  Prelude.NFData
    CreateObservabilityConfiguration
  where
  rnf CreateObservabilityConfiguration' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf traceConfiguration
      `Prelude.seq` Prelude.rnf observabilityConfigurationName

instance
  Data.ToHeaders
    CreateObservabilityConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AppRunner.CreateObservabilityConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateObservabilityConfiguration where
  toJSON CreateObservabilityConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("TraceConfiguration" Data..=)
              Prelude.<$> traceConfiguration,
            Prelude.Just
              ( "ObservabilityConfigurationName"
                  Data..= observabilityConfigurationName
              )
          ]
      )

instance Data.ToPath CreateObservabilityConfiguration where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateObservabilityConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateObservabilityConfigurationResponse' smart constructor.
data CreateObservabilityConfigurationResponse = CreateObservabilityConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A description of the App Runner observability configuration that\'s
    -- created by this request.
    observabilityConfiguration :: ObservabilityConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateObservabilityConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createObservabilityConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'observabilityConfiguration', 'createObservabilityConfigurationResponse_observabilityConfiguration' - A description of the App Runner observability configuration that\'s
-- created by this request.
newCreateObservabilityConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'observabilityConfiguration'
  ObservabilityConfiguration ->
  CreateObservabilityConfigurationResponse
newCreateObservabilityConfigurationResponse
  pHttpStatus_
  pObservabilityConfiguration_ =
    CreateObservabilityConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        observabilityConfiguration =
          pObservabilityConfiguration_
      }

-- | The response's http status code.
createObservabilityConfigurationResponse_httpStatus :: Lens.Lens' CreateObservabilityConfigurationResponse Prelude.Int
createObservabilityConfigurationResponse_httpStatus = Lens.lens (\CreateObservabilityConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateObservabilityConfigurationResponse' {} a -> s {httpStatus = a} :: CreateObservabilityConfigurationResponse)

-- | A description of the App Runner observability configuration that\'s
-- created by this request.
createObservabilityConfigurationResponse_observabilityConfiguration :: Lens.Lens' CreateObservabilityConfigurationResponse ObservabilityConfiguration
createObservabilityConfigurationResponse_observabilityConfiguration = Lens.lens (\CreateObservabilityConfigurationResponse' {observabilityConfiguration} -> observabilityConfiguration) (\s@CreateObservabilityConfigurationResponse' {} a -> s {observabilityConfiguration = a} :: CreateObservabilityConfigurationResponse)

instance
  Prelude.NFData
    CreateObservabilityConfigurationResponse
  where
  rnf CreateObservabilityConfigurationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf observabilityConfiguration
