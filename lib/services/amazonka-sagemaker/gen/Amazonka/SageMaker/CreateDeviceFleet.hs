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
-- Module      : Amazonka.SageMaker.CreateDeviceFleet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a device fleet.
module Amazonka.SageMaker.CreateDeviceFleet
  ( -- * Creating a Request
    CreateDeviceFleet (..),
    newCreateDeviceFleet,

    -- * Request Lenses
    createDeviceFleet_description,
    createDeviceFleet_enableIotRoleAlias,
    createDeviceFleet_roleArn,
    createDeviceFleet_tags,
    createDeviceFleet_deviceFleetName,
    createDeviceFleet_outputConfig,

    -- * Destructuring the Response
    CreateDeviceFleetResponse (..),
    newCreateDeviceFleetResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateDeviceFleet' smart constructor.
data CreateDeviceFleet = CreateDeviceFleet'
  { -- | A description of the fleet.
    description :: Prelude.Maybe Prelude.Text,
    -- | Whether to create an Amazon Web Services IoT Role Alias during device
    -- fleet creation. The name of the role alias generated will match this
    -- pattern: \"SageMakerEdge-{DeviceFleetName}\".
    --
    -- For example, if your device fleet is called \"demo-fleet\", the name of
    -- the role alias will be \"SageMakerEdge-demo-fleet\".
    enableIotRoleAlias :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) that has access to Amazon Web Services
    -- Internet of Things (IoT).
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Creates tags for the specified fleet.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the fleet that the device belongs to.
    deviceFleetName :: Prelude.Text,
    -- | The output configuration for storing sample data collected by the fleet.
    outputConfig :: EdgeOutputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeviceFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createDeviceFleet_description' - A description of the fleet.
--
-- 'enableIotRoleAlias', 'createDeviceFleet_enableIotRoleAlias' - Whether to create an Amazon Web Services IoT Role Alias during device
-- fleet creation. The name of the role alias generated will match this
-- pattern: \"SageMakerEdge-{DeviceFleetName}\".
--
-- For example, if your device fleet is called \"demo-fleet\", the name of
-- the role alias will be \"SageMakerEdge-demo-fleet\".
--
-- 'roleArn', 'createDeviceFleet_roleArn' - The Amazon Resource Name (ARN) that has access to Amazon Web Services
-- Internet of Things (IoT).
--
-- 'tags', 'createDeviceFleet_tags' - Creates tags for the specified fleet.
--
-- 'deviceFleetName', 'createDeviceFleet_deviceFleetName' - The name of the fleet that the device belongs to.
--
-- 'outputConfig', 'createDeviceFleet_outputConfig' - The output configuration for storing sample data collected by the fleet.
newCreateDeviceFleet ::
  -- | 'deviceFleetName'
  Prelude.Text ->
  -- | 'outputConfig'
  EdgeOutputConfig ->
  CreateDeviceFleet
newCreateDeviceFleet pDeviceFleetName_ pOutputConfig_ =
  CreateDeviceFleet'
    { description = Prelude.Nothing,
      enableIotRoleAlias = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      deviceFleetName = pDeviceFleetName_,
      outputConfig = pOutputConfig_
    }

-- | A description of the fleet.
createDeviceFleet_description :: Lens.Lens' CreateDeviceFleet (Prelude.Maybe Prelude.Text)
createDeviceFleet_description = Lens.lens (\CreateDeviceFleet' {description} -> description) (\s@CreateDeviceFleet' {} a -> s {description = a} :: CreateDeviceFleet)

-- | Whether to create an Amazon Web Services IoT Role Alias during device
-- fleet creation. The name of the role alias generated will match this
-- pattern: \"SageMakerEdge-{DeviceFleetName}\".
--
-- For example, if your device fleet is called \"demo-fleet\", the name of
-- the role alias will be \"SageMakerEdge-demo-fleet\".
createDeviceFleet_enableIotRoleAlias :: Lens.Lens' CreateDeviceFleet (Prelude.Maybe Prelude.Bool)
createDeviceFleet_enableIotRoleAlias = Lens.lens (\CreateDeviceFleet' {enableIotRoleAlias} -> enableIotRoleAlias) (\s@CreateDeviceFleet' {} a -> s {enableIotRoleAlias = a} :: CreateDeviceFleet)

-- | The Amazon Resource Name (ARN) that has access to Amazon Web Services
-- Internet of Things (IoT).
createDeviceFleet_roleArn :: Lens.Lens' CreateDeviceFleet (Prelude.Maybe Prelude.Text)
createDeviceFleet_roleArn = Lens.lens (\CreateDeviceFleet' {roleArn} -> roleArn) (\s@CreateDeviceFleet' {} a -> s {roleArn = a} :: CreateDeviceFleet)

-- | Creates tags for the specified fleet.
createDeviceFleet_tags :: Lens.Lens' CreateDeviceFleet (Prelude.Maybe [Tag])
createDeviceFleet_tags = Lens.lens (\CreateDeviceFleet' {tags} -> tags) (\s@CreateDeviceFleet' {} a -> s {tags = a} :: CreateDeviceFleet) Prelude.. Lens.mapping Lens.coerced

-- | The name of the fleet that the device belongs to.
createDeviceFleet_deviceFleetName :: Lens.Lens' CreateDeviceFleet Prelude.Text
createDeviceFleet_deviceFleetName = Lens.lens (\CreateDeviceFleet' {deviceFleetName} -> deviceFleetName) (\s@CreateDeviceFleet' {} a -> s {deviceFleetName = a} :: CreateDeviceFleet)

-- | The output configuration for storing sample data collected by the fleet.
createDeviceFleet_outputConfig :: Lens.Lens' CreateDeviceFleet EdgeOutputConfig
createDeviceFleet_outputConfig = Lens.lens (\CreateDeviceFleet' {outputConfig} -> outputConfig) (\s@CreateDeviceFleet' {} a -> s {outputConfig = a} :: CreateDeviceFleet)

instance Core.AWSRequest CreateDeviceFleet where
  type
    AWSResponse CreateDeviceFleet =
      CreateDeviceFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull CreateDeviceFleetResponse'

instance Prelude.Hashable CreateDeviceFleet where
  hashWithSalt _salt CreateDeviceFleet' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` enableIotRoleAlias
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` deviceFleetName
      `Prelude.hashWithSalt` outputConfig

instance Prelude.NFData CreateDeviceFleet where
  rnf CreateDeviceFleet' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf enableIotRoleAlias
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf deviceFleetName
      `Prelude.seq` Prelude.rnf outputConfig

instance Data.ToHeaders CreateDeviceFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateDeviceFleet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDeviceFleet where
  toJSON CreateDeviceFleet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("EnableIotRoleAlias" Data..=)
              Prelude.<$> enableIotRoleAlias,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("DeviceFleetName" Data..= deviceFleetName),
            Prelude.Just ("OutputConfig" Data..= outputConfig)
          ]
      )

instance Data.ToPath CreateDeviceFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDeviceFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDeviceFleetResponse' smart constructor.
data CreateDeviceFleetResponse = CreateDeviceFleetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeviceFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateDeviceFleetResponse ::
  CreateDeviceFleetResponse
newCreateDeviceFleetResponse =
  CreateDeviceFleetResponse'

instance Prelude.NFData CreateDeviceFleetResponse where
  rnf _ = ()
