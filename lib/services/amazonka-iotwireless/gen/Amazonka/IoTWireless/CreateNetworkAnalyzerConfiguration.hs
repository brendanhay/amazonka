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
-- Module      : Amazonka.IoTWireless.CreateNetworkAnalyzerConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new network analyzer configuration.
module Amazonka.IoTWireless.CreateNetworkAnalyzerConfiguration
  ( -- * Creating a Request
    CreateNetworkAnalyzerConfiguration (..),
    newCreateNetworkAnalyzerConfiguration,

    -- * Request Lenses
    createNetworkAnalyzerConfiguration_tags,
    createNetworkAnalyzerConfiguration_wirelessGateways,
    createNetworkAnalyzerConfiguration_clientRequestToken,
    createNetworkAnalyzerConfiguration_wirelessDevices,
    createNetworkAnalyzerConfiguration_description,
    createNetworkAnalyzerConfiguration_traceContent,
    createNetworkAnalyzerConfiguration_name,

    -- * Destructuring the Response
    CreateNetworkAnalyzerConfigurationResponse (..),
    newCreateNetworkAnalyzerConfigurationResponse,

    -- * Response Lenses
    createNetworkAnalyzerConfigurationResponse_name,
    createNetworkAnalyzerConfigurationResponse_arn,
    createNetworkAnalyzerConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateNetworkAnalyzerConfiguration' smart constructor.
data CreateNetworkAnalyzerConfiguration = CreateNetworkAnalyzerConfiguration'
  { tags :: Prelude.Maybe [Tag],
    -- | Wireless gateway resources to add to the network analyzer configuration.
    -- Provide the @WirelessGatewayId@ of the resource to add in the input
    -- array.
    wirelessGateways :: Prelude.Maybe [Prelude.Text],
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Wireless device resources to add to the network analyzer configuration.
    -- Provide the @WirelessDeviceId@ of the resource to add in the input
    -- array.
    wirelessDevices :: Prelude.Maybe [Prelude.Text],
    description :: Prelude.Maybe Prelude.Text,
    traceContent :: Prelude.Maybe TraceContent,
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNetworkAnalyzerConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createNetworkAnalyzerConfiguration_tags' - Undocumented member.
--
-- 'wirelessGateways', 'createNetworkAnalyzerConfiguration_wirelessGateways' - Wireless gateway resources to add to the network analyzer configuration.
-- Provide the @WirelessGatewayId@ of the resource to add in the input
-- array.
--
-- 'clientRequestToken', 'createNetworkAnalyzerConfiguration_clientRequestToken' - Undocumented member.
--
-- 'wirelessDevices', 'createNetworkAnalyzerConfiguration_wirelessDevices' - Wireless device resources to add to the network analyzer configuration.
-- Provide the @WirelessDeviceId@ of the resource to add in the input
-- array.
--
-- 'description', 'createNetworkAnalyzerConfiguration_description' - Undocumented member.
--
-- 'traceContent', 'createNetworkAnalyzerConfiguration_traceContent' - Undocumented member.
--
-- 'name', 'createNetworkAnalyzerConfiguration_name' - Undocumented member.
newCreateNetworkAnalyzerConfiguration ::
  -- | 'name'
  Prelude.Text ->
  CreateNetworkAnalyzerConfiguration
newCreateNetworkAnalyzerConfiguration pName_ =
  CreateNetworkAnalyzerConfiguration'
    { tags =
        Prelude.Nothing,
      wirelessGateways = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      wirelessDevices = Prelude.Nothing,
      description = Prelude.Nothing,
      traceContent = Prelude.Nothing,
      name = pName_
    }

-- | Undocumented member.
createNetworkAnalyzerConfiguration_tags :: Lens.Lens' CreateNetworkAnalyzerConfiguration (Prelude.Maybe [Tag])
createNetworkAnalyzerConfiguration_tags = Lens.lens (\CreateNetworkAnalyzerConfiguration' {tags} -> tags) (\s@CreateNetworkAnalyzerConfiguration' {} a -> s {tags = a} :: CreateNetworkAnalyzerConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Wireless gateway resources to add to the network analyzer configuration.
-- Provide the @WirelessGatewayId@ of the resource to add in the input
-- array.
createNetworkAnalyzerConfiguration_wirelessGateways :: Lens.Lens' CreateNetworkAnalyzerConfiguration (Prelude.Maybe [Prelude.Text])
createNetworkAnalyzerConfiguration_wirelessGateways = Lens.lens (\CreateNetworkAnalyzerConfiguration' {wirelessGateways} -> wirelessGateways) (\s@CreateNetworkAnalyzerConfiguration' {} a -> s {wirelessGateways = a} :: CreateNetworkAnalyzerConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createNetworkAnalyzerConfiguration_clientRequestToken :: Lens.Lens' CreateNetworkAnalyzerConfiguration (Prelude.Maybe Prelude.Text)
createNetworkAnalyzerConfiguration_clientRequestToken = Lens.lens (\CreateNetworkAnalyzerConfiguration' {clientRequestToken} -> clientRequestToken) (\s@CreateNetworkAnalyzerConfiguration' {} a -> s {clientRequestToken = a} :: CreateNetworkAnalyzerConfiguration)

-- | Wireless device resources to add to the network analyzer configuration.
-- Provide the @WirelessDeviceId@ of the resource to add in the input
-- array.
createNetworkAnalyzerConfiguration_wirelessDevices :: Lens.Lens' CreateNetworkAnalyzerConfiguration (Prelude.Maybe [Prelude.Text])
createNetworkAnalyzerConfiguration_wirelessDevices = Lens.lens (\CreateNetworkAnalyzerConfiguration' {wirelessDevices} -> wirelessDevices) (\s@CreateNetworkAnalyzerConfiguration' {} a -> s {wirelessDevices = a} :: CreateNetworkAnalyzerConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createNetworkAnalyzerConfiguration_description :: Lens.Lens' CreateNetworkAnalyzerConfiguration (Prelude.Maybe Prelude.Text)
createNetworkAnalyzerConfiguration_description = Lens.lens (\CreateNetworkAnalyzerConfiguration' {description} -> description) (\s@CreateNetworkAnalyzerConfiguration' {} a -> s {description = a} :: CreateNetworkAnalyzerConfiguration)

-- | Undocumented member.
createNetworkAnalyzerConfiguration_traceContent :: Lens.Lens' CreateNetworkAnalyzerConfiguration (Prelude.Maybe TraceContent)
createNetworkAnalyzerConfiguration_traceContent = Lens.lens (\CreateNetworkAnalyzerConfiguration' {traceContent} -> traceContent) (\s@CreateNetworkAnalyzerConfiguration' {} a -> s {traceContent = a} :: CreateNetworkAnalyzerConfiguration)

-- | Undocumented member.
createNetworkAnalyzerConfiguration_name :: Lens.Lens' CreateNetworkAnalyzerConfiguration Prelude.Text
createNetworkAnalyzerConfiguration_name = Lens.lens (\CreateNetworkAnalyzerConfiguration' {name} -> name) (\s@CreateNetworkAnalyzerConfiguration' {} a -> s {name = a} :: CreateNetworkAnalyzerConfiguration)

instance
  Core.AWSRequest
    CreateNetworkAnalyzerConfiguration
  where
  type
    AWSResponse CreateNetworkAnalyzerConfiguration =
      CreateNetworkAnalyzerConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNetworkAnalyzerConfigurationResponse'
            Prelude.<$> (x Data..?> "Name") Prelude.<*> (x Data..?> "Arn")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateNetworkAnalyzerConfiguration
  where
  hashWithSalt
    _salt
    CreateNetworkAnalyzerConfiguration' {..} =
      _salt `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` wirelessGateways
        `Prelude.hashWithSalt` clientRequestToken
        `Prelude.hashWithSalt` wirelessDevices
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` traceContent
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    CreateNetworkAnalyzerConfiguration
  where
  rnf CreateNetworkAnalyzerConfiguration' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf wirelessGateways
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf wirelessDevices
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf traceContent
      `Prelude.seq` Prelude.rnf name

instance
  Data.ToHeaders
    CreateNetworkAnalyzerConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    CreateNetworkAnalyzerConfiguration
  where
  toJSON CreateNetworkAnalyzerConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("WirelessGateways" Data..=)
              Prelude.<$> wirelessGateways,
            ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("WirelessDevices" Data..=)
              Prelude.<$> wirelessDevices,
            ("Description" Data..=) Prelude.<$> description,
            ("TraceContent" Data..=) Prelude.<$> traceContent,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance
  Data.ToPath
    CreateNetworkAnalyzerConfiguration
  where
  toPath =
    Prelude.const "/network-analyzer-configurations"

instance
  Data.ToQuery
    CreateNetworkAnalyzerConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateNetworkAnalyzerConfigurationResponse' smart constructor.
data CreateNetworkAnalyzerConfigurationResponse = CreateNetworkAnalyzerConfigurationResponse'
  { name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name of the new resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNetworkAnalyzerConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createNetworkAnalyzerConfigurationResponse_name' - Undocumented member.
--
-- 'arn', 'createNetworkAnalyzerConfigurationResponse_arn' - The Amazon Resource Name of the new resource.
--
-- 'httpStatus', 'createNetworkAnalyzerConfigurationResponse_httpStatus' - The response's http status code.
newCreateNetworkAnalyzerConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateNetworkAnalyzerConfigurationResponse
newCreateNetworkAnalyzerConfigurationResponse
  pHttpStatus_ =
    CreateNetworkAnalyzerConfigurationResponse'
      { name =
          Prelude.Nothing,
        arn = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
createNetworkAnalyzerConfigurationResponse_name :: Lens.Lens' CreateNetworkAnalyzerConfigurationResponse (Prelude.Maybe Prelude.Text)
createNetworkAnalyzerConfigurationResponse_name = Lens.lens (\CreateNetworkAnalyzerConfigurationResponse' {name} -> name) (\s@CreateNetworkAnalyzerConfigurationResponse' {} a -> s {name = a} :: CreateNetworkAnalyzerConfigurationResponse)

-- | The Amazon Resource Name of the new resource.
createNetworkAnalyzerConfigurationResponse_arn :: Lens.Lens' CreateNetworkAnalyzerConfigurationResponse (Prelude.Maybe Prelude.Text)
createNetworkAnalyzerConfigurationResponse_arn = Lens.lens (\CreateNetworkAnalyzerConfigurationResponse' {arn} -> arn) (\s@CreateNetworkAnalyzerConfigurationResponse' {} a -> s {arn = a} :: CreateNetworkAnalyzerConfigurationResponse)

-- | The response's http status code.
createNetworkAnalyzerConfigurationResponse_httpStatus :: Lens.Lens' CreateNetworkAnalyzerConfigurationResponse Prelude.Int
createNetworkAnalyzerConfigurationResponse_httpStatus = Lens.lens (\CreateNetworkAnalyzerConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateNetworkAnalyzerConfigurationResponse' {} a -> s {httpStatus = a} :: CreateNetworkAnalyzerConfigurationResponse)

instance
  Prelude.NFData
    CreateNetworkAnalyzerConfigurationResponse
  where
  rnf CreateNetworkAnalyzerConfigurationResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
