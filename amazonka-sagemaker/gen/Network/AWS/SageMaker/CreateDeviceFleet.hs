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
-- Module      : Network.AWS.SageMaker.CreateDeviceFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a device fleet.
module Network.AWS.SageMaker.CreateDeviceFleet
  ( -- * Creating a Request
    CreateDeviceFleet (..),
    newCreateDeviceFleet,

    -- * Request Lenses
    createDeviceFleet_roleArn,
    createDeviceFleet_tags,
    createDeviceFleet_description,
    createDeviceFleet_deviceFleetName,
    createDeviceFleet_outputConfig,

    -- * Destructuring the Response
    CreateDeviceFleetResponse (..),
    newCreateDeviceFleetResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateDeviceFleet' smart constructor.
data CreateDeviceFleet = CreateDeviceFleet'
  { -- | The Amazon Resource Name (ARN) that has access to AWS Internet of Things
    -- (IoT).
    roleArn :: Core.Maybe Core.Text,
    -- | Creates tags for the specified fleet.
    tags :: Core.Maybe [Tag],
    -- | A description of the fleet.
    description :: Core.Maybe Core.Text,
    -- | The name of the fleet that the device belongs to.
    deviceFleetName :: Core.Text,
    -- | The output configuration for storing sample data collected by the fleet.
    outputConfig :: EdgeOutputConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDeviceFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'createDeviceFleet_roleArn' - The Amazon Resource Name (ARN) that has access to AWS Internet of Things
-- (IoT).
--
-- 'tags', 'createDeviceFleet_tags' - Creates tags for the specified fleet.
--
-- 'description', 'createDeviceFleet_description' - A description of the fleet.
--
-- 'deviceFleetName', 'createDeviceFleet_deviceFleetName' - The name of the fleet that the device belongs to.
--
-- 'outputConfig', 'createDeviceFleet_outputConfig' - The output configuration for storing sample data collected by the fleet.
newCreateDeviceFleet ::
  -- | 'deviceFleetName'
  Core.Text ->
  -- | 'outputConfig'
  EdgeOutputConfig ->
  CreateDeviceFleet
newCreateDeviceFleet pDeviceFleetName_ pOutputConfig_ =
  CreateDeviceFleet'
    { roleArn = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      deviceFleetName = pDeviceFleetName_,
      outputConfig = pOutputConfig_
    }

-- | The Amazon Resource Name (ARN) that has access to AWS Internet of Things
-- (IoT).
createDeviceFleet_roleArn :: Lens.Lens' CreateDeviceFleet (Core.Maybe Core.Text)
createDeviceFleet_roleArn = Lens.lens (\CreateDeviceFleet' {roleArn} -> roleArn) (\s@CreateDeviceFleet' {} a -> s {roleArn = a} :: CreateDeviceFleet)

-- | Creates tags for the specified fleet.
createDeviceFleet_tags :: Lens.Lens' CreateDeviceFleet (Core.Maybe [Tag])
createDeviceFleet_tags = Lens.lens (\CreateDeviceFleet' {tags} -> tags) (\s@CreateDeviceFleet' {} a -> s {tags = a} :: CreateDeviceFleet) Core.. Lens.mapping Lens._Coerce

-- | A description of the fleet.
createDeviceFleet_description :: Lens.Lens' CreateDeviceFleet (Core.Maybe Core.Text)
createDeviceFleet_description = Lens.lens (\CreateDeviceFleet' {description} -> description) (\s@CreateDeviceFleet' {} a -> s {description = a} :: CreateDeviceFleet)

-- | The name of the fleet that the device belongs to.
createDeviceFleet_deviceFleetName :: Lens.Lens' CreateDeviceFleet Core.Text
createDeviceFleet_deviceFleetName = Lens.lens (\CreateDeviceFleet' {deviceFleetName} -> deviceFleetName) (\s@CreateDeviceFleet' {} a -> s {deviceFleetName = a} :: CreateDeviceFleet)

-- | The output configuration for storing sample data collected by the fleet.
createDeviceFleet_outputConfig :: Lens.Lens' CreateDeviceFleet EdgeOutputConfig
createDeviceFleet_outputConfig = Lens.lens (\CreateDeviceFleet' {outputConfig} -> outputConfig) (\s@CreateDeviceFleet' {} a -> s {outputConfig = a} :: CreateDeviceFleet)

instance Core.AWSRequest CreateDeviceFleet where
  type
    AWSResponse CreateDeviceFleet =
      CreateDeviceFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull CreateDeviceFleetResponse'

instance Core.Hashable CreateDeviceFleet

instance Core.NFData CreateDeviceFleet

instance Core.ToHeaders CreateDeviceFleet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.CreateDeviceFleet" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateDeviceFleet where
  toJSON CreateDeviceFleet' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoleArn" Core..=) Core.<$> roleArn,
            ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            Core.Just
              ("DeviceFleetName" Core..= deviceFleetName),
            Core.Just ("OutputConfig" Core..= outputConfig)
          ]
      )

instance Core.ToPath CreateDeviceFleet where
  toPath = Core.const "/"

instance Core.ToQuery CreateDeviceFleet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateDeviceFleetResponse' smart constructor.
data CreateDeviceFleetResponse = CreateDeviceFleetResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDeviceFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateDeviceFleetResponse ::
  CreateDeviceFleetResponse
newCreateDeviceFleetResponse =
  CreateDeviceFleetResponse'

instance Core.NFData CreateDeviceFleetResponse
