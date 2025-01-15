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
-- Module      : Amazonka.AppRunner.CreateAutoScalingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an App Runner automatic scaling configuration resource. App
-- Runner requires this resource when you create or update App Runner
-- services and you require non-default auto scaling settings. You can
-- share an auto scaling configuration across multiple services.
--
-- Create multiple revisions of a configuration by calling this action
-- multiple times using the same @AutoScalingConfigurationName@. The call
-- returns incremental @AutoScalingConfigurationRevision@ values. When you
-- create a service and configure an auto scaling configuration resource,
-- the service uses the latest active revision of the auto scaling
-- configuration by default. You can optionally configure the service to
-- use a specific revision.
--
-- Configure a higher @MinSize@ to increase the spread of your App Runner
-- service over more Availability Zones in the Amazon Web Services Region.
-- The tradeoff is a higher minimal cost.
--
-- Configure a lower @MaxSize@ to control your cost. The tradeoff is lower
-- responsiveness during peak demand.
module Amazonka.AppRunner.CreateAutoScalingConfiguration
  ( -- * Creating a Request
    CreateAutoScalingConfiguration (..),
    newCreateAutoScalingConfiguration,

    -- * Request Lenses
    createAutoScalingConfiguration_maxConcurrency,
    createAutoScalingConfiguration_maxSize,
    createAutoScalingConfiguration_minSize,
    createAutoScalingConfiguration_tags,
    createAutoScalingConfiguration_autoScalingConfigurationName,

    -- * Destructuring the Response
    CreateAutoScalingConfigurationResponse (..),
    newCreateAutoScalingConfigurationResponse,

    -- * Response Lenses
    createAutoScalingConfigurationResponse_httpStatus,
    createAutoScalingConfigurationResponse_autoScalingConfiguration,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAutoScalingConfiguration' smart constructor.
data CreateAutoScalingConfiguration = CreateAutoScalingConfiguration'
  { -- | The maximum number of concurrent requests that you want an instance to
    -- process. If the number of concurrent requests exceeds this limit, App
    -- Runner scales up your service.
    --
    -- Default: @100@
    maxConcurrency :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of instances that your service scales up to. At most
    -- @MaxSize@ instances actively serve traffic for your service.
    --
    -- Default: @25@
    maxSize :: Prelude.Maybe Prelude.Natural,
    -- | The minimum number of instances that App Runner provisions for your
    -- service. The service always has at least @MinSize@ provisioned
    -- instances. Some of them actively serve traffic. The rest of them
    -- (provisioned and inactive instances) are a cost-effective compute
    -- capacity reserve and are ready to be quickly activated. You pay for
    -- memory usage of all the provisioned instances. You pay for CPU usage of
    -- only the active subset.
    --
    -- App Runner temporarily doubles the number of provisioned instances
    -- during deployments, to maintain the same capacity for both old and new
    -- code.
    --
    -- Default: @1@
    minSize :: Prelude.Maybe Prelude.Natural,
    -- | A list of metadata items that you can associate with your auto scaling
    -- configuration resource. A tag is a key-value pair.
    tags :: Prelude.Maybe [Tag],
    -- | A name for the auto scaling configuration. When you use it for the first
    -- time in an Amazon Web Services Region, App Runner creates revision
    -- number @1@ of this name. When you use the same name in subsequent calls,
    -- App Runner creates incremental revisions of the configuration.
    --
    -- The name @DefaultConfiguration@ is reserved (it\'s the configuration
    -- that App Runner uses if you don\'t provide a custome one). You can\'t
    -- use it to create a new auto scaling configuration, and you can\'t create
    -- a revision of it.
    --
    -- When you want to use your own auto scaling configuration for your App
    -- Runner service, /create a configuration with a different name/, and then
    -- provide it when you create or update your service.
    autoScalingConfigurationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAutoScalingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxConcurrency', 'createAutoScalingConfiguration_maxConcurrency' - The maximum number of concurrent requests that you want an instance to
-- process. If the number of concurrent requests exceeds this limit, App
-- Runner scales up your service.
--
-- Default: @100@
--
-- 'maxSize', 'createAutoScalingConfiguration_maxSize' - The maximum number of instances that your service scales up to. At most
-- @MaxSize@ instances actively serve traffic for your service.
--
-- Default: @25@
--
-- 'minSize', 'createAutoScalingConfiguration_minSize' - The minimum number of instances that App Runner provisions for your
-- service. The service always has at least @MinSize@ provisioned
-- instances. Some of them actively serve traffic. The rest of them
-- (provisioned and inactive instances) are a cost-effective compute
-- capacity reserve and are ready to be quickly activated. You pay for
-- memory usage of all the provisioned instances. You pay for CPU usage of
-- only the active subset.
--
-- App Runner temporarily doubles the number of provisioned instances
-- during deployments, to maintain the same capacity for both old and new
-- code.
--
-- Default: @1@
--
-- 'tags', 'createAutoScalingConfiguration_tags' - A list of metadata items that you can associate with your auto scaling
-- configuration resource. A tag is a key-value pair.
--
-- 'autoScalingConfigurationName', 'createAutoScalingConfiguration_autoScalingConfigurationName' - A name for the auto scaling configuration. When you use it for the first
-- time in an Amazon Web Services Region, App Runner creates revision
-- number @1@ of this name. When you use the same name in subsequent calls,
-- App Runner creates incremental revisions of the configuration.
--
-- The name @DefaultConfiguration@ is reserved (it\'s the configuration
-- that App Runner uses if you don\'t provide a custome one). You can\'t
-- use it to create a new auto scaling configuration, and you can\'t create
-- a revision of it.
--
-- When you want to use your own auto scaling configuration for your App
-- Runner service, /create a configuration with a different name/, and then
-- provide it when you create or update your service.
newCreateAutoScalingConfiguration ::
  -- | 'autoScalingConfigurationName'
  Prelude.Text ->
  CreateAutoScalingConfiguration
newCreateAutoScalingConfiguration
  pAutoScalingConfigurationName_ =
    CreateAutoScalingConfiguration'
      { maxConcurrency =
          Prelude.Nothing,
        maxSize = Prelude.Nothing,
        minSize = Prelude.Nothing,
        tags = Prelude.Nothing,
        autoScalingConfigurationName =
          pAutoScalingConfigurationName_
      }

-- | The maximum number of concurrent requests that you want an instance to
-- process. If the number of concurrent requests exceeds this limit, App
-- Runner scales up your service.
--
-- Default: @100@
createAutoScalingConfiguration_maxConcurrency :: Lens.Lens' CreateAutoScalingConfiguration (Prelude.Maybe Prelude.Natural)
createAutoScalingConfiguration_maxConcurrency = Lens.lens (\CreateAutoScalingConfiguration' {maxConcurrency} -> maxConcurrency) (\s@CreateAutoScalingConfiguration' {} a -> s {maxConcurrency = a} :: CreateAutoScalingConfiguration)

-- | The maximum number of instances that your service scales up to. At most
-- @MaxSize@ instances actively serve traffic for your service.
--
-- Default: @25@
createAutoScalingConfiguration_maxSize :: Lens.Lens' CreateAutoScalingConfiguration (Prelude.Maybe Prelude.Natural)
createAutoScalingConfiguration_maxSize = Lens.lens (\CreateAutoScalingConfiguration' {maxSize} -> maxSize) (\s@CreateAutoScalingConfiguration' {} a -> s {maxSize = a} :: CreateAutoScalingConfiguration)

-- | The minimum number of instances that App Runner provisions for your
-- service. The service always has at least @MinSize@ provisioned
-- instances. Some of them actively serve traffic. The rest of them
-- (provisioned and inactive instances) are a cost-effective compute
-- capacity reserve and are ready to be quickly activated. You pay for
-- memory usage of all the provisioned instances. You pay for CPU usage of
-- only the active subset.
--
-- App Runner temporarily doubles the number of provisioned instances
-- during deployments, to maintain the same capacity for both old and new
-- code.
--
-- Default: @1@
createAutoScalingConfiguration_minSize :: Lens.Lens' CreateAutoScalingConfiguration (Prelude.Maybe Prelude.Natural)
createAutoScalingConfiguration_minSize = Lens.lens (\CreateAutoScalingConfiguration' {minSize} -> minSize) (\s@CreateAutoScalingConfiguration' {} a -> s {minSize = a} :: CreateAutoScalingConfiguration)

-- | A list of metadata items that you can associate with your auto scaling
-- configuration resource. A tag is a key-value pair.
createAutoScalingConfiguration_tags :: Lens.Lens' CreateAutoScalingConfiguration (Prelude.Maybe [Tag])
createAutoScalingConfiguration_tags = Lens.lens (\CreateAutoScalingConfiguration' {tags} -> tags) (\s@CreateAutoScalingConfiguration' {} a -> s {tags = a} :: CreateAutoScalingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A name for the auto scaling configuration. When you use it for the first
-- time in an Amazon Web Services Region, App Runner creates revision
-- number @1@ of this name. When you use the same name in subsequent calls,
-- App Runner creates incremental revisions of the configuration.
--
-- The name @DefaultConfiguration@ is reserved (it\'s the configuration
-- that App Runner uses if you don\'t provide a custome one). You can\'t
-- use it to create a new auto scaling configuration, and you can\'t create
-- a revision of it.
--
-- When you want to use your own auto scaling configuration for your App
-- Runner service, /create a configuration with a different name/, and then
-- provide it when you create or update your service.
createAutoScalingConfiguration_autoScalingConfigurationName :: Lens.Lens' CreateAutoScalingConfiguration Prelude.Text
createAutoScalingConfiguration_autoScalingConfigurationName = Lens.lens (\CreateAutoScalingConfiguration' {autoScalingConfigurationName} -> autoScalingConfigurationName) (\s@CreateAutoScalingConfiguration' {} a -> s {autoScalingConfigurationName = a} :: CreateAutoScalingConfiguration)

instance
  Core.AWSRequest
    CreateAutoScalingConfiguration
  where
  type
    AWSResponse CreateAutoScalingConfiguration =
      CreateAutoScalingConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAutoScalingConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "AutoScalingConfiguration")
      )

instance
  Prelude.Hashable
    CreateAutoScalingConfiguration
  where
  hashWithSalt
    _salt
    CreateAutoScalingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` maxConcurrency
        `Prelude.hashWithSalt` maxSize
        `Prelude.hashWithSalt` minSize
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` autoScalingConfigurationName

instance
  Prelude.NFData
    CreateAutoScalingConfiguration
  where
  rnf CreateAutoScalingConfiguration' {..} =
    Prelude.rnf maxConcurrency `Prelude.seq`
      Prelude.rnf maxSize `Prelude.seq`
        Prelude.rnf minSize `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf autoScalingConfigurationName

instance
  Data.ToHeaders
    CreateAutoScalingConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AppRunner.CreateAutoScalingConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAutoScalingConfiguration where
  toJSON CreateAutoScalingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxConcurrency" Data..=)
              Prelude.<$> maxConcurrency,
            ("MaxSize" Data..=) Prelude.<$> maxSize,
            ("MinSize" Data..=) Prelude.<$> minSize,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "AutoScalingConfigurationName"
                  Data..= autoScalingConfigurationName
              )
          ]
      )

instance Data.ToPath CreateAutoScalingConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAutoScalingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAutoScalingConfigurationResponse' smart constructor.
data CreateAutoScalingConfigurationResponse = CreateAutoScalingConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A description of the App Runner auto scaling configuration that\'s
    -- created by this request.
    autoScalingConfiguration :: AutoScalingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAutoScalingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAutoScalingConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'autoScalingConfiguration', 'createAutoScalingConfigurationResponse_autoScalingConfiguration' - A description of the App Runner auto scaling configuration that\'s
-- created by this request.
newCreateAutoScalingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'autoScalingConfiguration'
  AutoScalingConfiguration ->
  CreateAutoScalingConfigurationResponse
newCreateAutoScalingConfigurationResponse
  pHttpStatus_
  pAutoScalingConfiguration_ =
    CreateAutoScalingConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        autoScalingConfiguration =
          pAutoScalingConfiguration_
      }

-- | The response's http status code.
createAutoScalingConfigurationResponse_httpStatus :: Lens.Lens' CreateAutoScalingConfigurationResponse Prelude.Int
createAutoScalingConfigurationResponse_httpStatus = Lens.lens (\CreateAutoScalingConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateAutoScalingConfigurationResponse' {} a -> s {httpStatus = a} :: CreateAutoScalingConfigurationResponse)

-- | A description of the App Runner auto scaling configuration that\'s
-- created by this request.
createAutoScalingConfigurationResponse_autoScalingConfiguration :: Lens.Lens' CreateAutoScalingConfigurationResponse AutoScalingConfiguration
createAutoScalingConfigurationResponse_autoScalingConfiguration = Lens.lens (\CreateAutoScalingConfigurationResponse' {autoScalingConfiguration} -> autoScalingConfiguration) (\s@CreateAutoScalingConfigurationResponse' {} a -> s {autoScalingConfiguration = a} :: CreateAutoScalingConfigurationResponse)

instance
  Prelude.NFData
    CreateAutoScalingConfigurationResponse
  where
  rnf CreateAutoScalingConfigurationResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf autoScalingConfiguration
