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
-- Module      : Amazonka.DirectConnect.DescribeRouterConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Details about the router.
module Amazonka.DirectConnect.DescribeRouterConfiguration
  ( -- * Creating a Request
    DescribeRouterConfiguration (..),
    newDescribeRouterConfiguration,

    -- * Request Lenses
    describeRouterConfiguration_routerTypeIdentifier,
    describeRouterConfiguration_virtualInterfaceId,

    -- * Destructuring the Response
    DescribeRouterConfigurationResponse (..),
    newDescribeRouterConfigurationResponse,

    -- * Response Lenses
    describeRouterConfigurationResponse_customerRouterConfig,
    describeRouterConfigurationResponse_router,
    describeRouterConfigurationResponse_virtualInterfaceId,
    describeRouterConfigurationResponse_virtualInterfaceName,
    describeRouterConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Provides the details about a virtual interface\'s router.
--
-- /See:/ 'newDescribeRouterConfiguration' smart constructor.
data DescribeRouterConfiguration = DescribeRouterConfiguration'
  { -- | Identifies the router by a combination of vendor, platform, and software
    -- version. For example, @CiscoSystemsInc-2900SeriesRouters-IOS124@.
    routerTypeIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRouterConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routerTypeIdentifier', 'describeRouterConfiguration_routerTypeIdentifier' - Identifies the router by a combination of vendor, platform, and software
-- version. For example, @CiscoSystemsInc-2900SeriesRouters-IOS124@.
--
-- 'virtualInterfaceId', 'describeRouterConfiguration_virtualInterfaceId' - The ID of the virtual interface.
newDescribeRouterConfiguration ::
  -- | 'virtualInterfaceId'
  Prelude.Text ->
  DescribeRouterConfiguration
newDescribeRouterConfiguration pVirtualInterfaceId_ =
  DescribeRouterConfiguration'
    { routerTypeIdentifier =
        Prelude.Nothing,
      virtualInterfaceId = pVirtualInterfaceId_
    }

-- | Identifies the router by a combination of vendor, platform, and software
-- version. For example, @CiscoSystemsInc-2900SeriesRouters-IOS124@.
describeRouterConfiguration_routerTypeIdentifier :: Lens.Lens' DescribeRouterConfiguration (Prelude.Maybe Prelude.Text)
describeRouterConfiguration_routerTypeIdentifier = Lens.lens (\DescribeRouterConfiguration' {routerTypeIdentifier} -> routerTypeIdentifier) (\s@DescribeRouterConfiguration' {} a -> s {routerTypeIdentifier = a} :: DescribeRouterConfiguration)

-- | The ID of the virtual interface.
describeRouterConfiguration_virtualInterfaceId :: Lens.Lens' DescribeRouterConfiguration Prelude.Text
describeRouterConfiguration_virtualInterfaceId = Lens.lens (\DescribeRouterConfiguration' {virtualInterfaceId} -> virtualInterfaceId) (\s@DescribeRouterConfiguration' {} a -> s {virtualInterfaceId = a} :: DescribeRouterConfiguration)

instance Core.AWSRequest DescribeRouterConfiguration where
  type
    AWSResponse DescribeRouterConfiguration =
      DescribeRouterConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRouterConfigurationResponse'
            Prelude.<$> (x Data..?> "customerRouterConfig")
            Prelude.<*> (x Data..?> "router")
            Prelude.<*> (x Data..?> "virtualInterfaceId")
            Prelude.<*> (x Data..?> "virtualInterfaceName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRouterConfiguration where
  hashWithSalt _salt DescribeRouterConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` routerTypeIdentifier
      `Prelude.hashWithSalt` virtualInterfaceId

instance Prelude.NFData DescribeRouterConfiguration where
  rnf DescribeRouterConfiguration' {..} =
    Prelude.rnf routerTypeIdentifier
      `Prelude.seq` Prelude.rnf virtualInterfaceId

instance Data.ToHeaders DescribeRouterConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.DescribeRouterConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRouterConfiguration where
  toJSON DescribeRouterConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("routerTypeIdentifier" Data..=)
              Prelude.<$> routerTypeIdentifier,
            Prelude.Just
              ("virtualInterfaceId" Data..= virtualInterfaceId)
          ]
      )

instance Data.ToPath DescribeRouterConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeRouterConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRouterConfigurationResponse' smart constructor.
data DescribeRouterConfigurationResponse = DescribeRouterConfigurationResponse'
  { -- | The customer router configuration.
    customerRouterConfig :: Prelude.Maybe Prelude.Text,
    -- | The details about the router.
    router :: Prelude.Maybe RouterType,
    -- | The ID assigned to the virtual interface.
    virtualInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | Provides the details about a virtual interface\'s router.
    virtualInterfaceName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRouterConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerRouterConfig', 'describeRouterConfigurationResponse_customerRouterConfig' - The customer router configuration.
--
-- 'router', 'describeRouterConfigurationResponse_router' - The details about the router.
--
-- 'virtualInterfaceId', 'describeRouterConfigurationResponse_virtualInterfaceId' - The ID assigned to the virtual interface.
--
-- 'virtualInterfaceName', 'describeRouterConfigurationResponse_virtualInterfaceName' - Provides the details about a virtual interface\'s router.
--
-- 'httpStatus', 'describeRouterConfigurationResponse_httpStatus' - The response's http status code.
newDescribeRouterConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRouterConfigurationResponse
newDescribeRouterConfigurationResponse pHttpStatus_ =
  DescribeRouterConfigurationResponse'
    { customerRouterConfig =
        Prelude.Nothing,
      router = Prelude.Nothing,
      virtualInterfaceId = Prelude.Nothing,
      virtualInterfaceName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The customer router configuration.
describeRouterConfigurationResponse_customerRouterConfig :: Lens.Lens' DescribeRouterConfigurationResponse (Prelude.Maybe Prelude.Text)
describeRouterConfigurationResponse_customerRouterConfig = Lens.lens (\DescribeRouterConfigurationResponse' {customerRouterConfig} -> customerRouterConfig) (\s@DescribeRouterConfigurationResponse' {} a -> s {customerRouterConfig = a} :: DescribeRouterConfigurationResponse)

-- | The details about the router.
describeRouterConfigurationResponse_router :: Lens.Lens' DescribeRouterConfigurationResponse (Prelude.Maybe RouterType)
describeRouterConfigurationResponse_router = Lens.lens (\DescribeRouterConfigurationResponse' {router} -> router) (\s@DescribeRouterConfigurationResponse' {} a -> s {router = a} :: DescribeRouterConfigurationResponse)

-- | The ID assigned to the virtual interface.
describeRouterConfigurationResponse_virtualInterfaceId :: Lens.Lens' DescribeRouterConfigurationResponse (Prelude.Maybe Prelude.Text)
describeRouterConfigurationResponse_virtualInterfaceId = Lens.lens (\DescribeRouterConfigurationResponse' {virtualInterfaceId} -> virtualInterfaceId) (\s@DescribeRouterConfigurationResponse' {} a -> s {virtualInterfaceId = a} :: DescribeRouterConfigurationResponse)

-- | Provides the details about a virtual interface\'s router.
describeRouterConfigurationResponse_virtualInterfaceName :: Lens.Lens' DescribeRouterConfigurationResponse (Prelude.Maybe Prelude.Text)
describeRouterConfigurationResponse_virtualInterfaceName = Lens.lens (\DescribeRouterConfigurationResponse' {virtualInterfaceName} -> virtualInterfaceName) (\s@DescribeRouterConfigurationResponse' {} a -> s {virtualInterfaceName = a} :: DescribeRouterConfigurationResponse)

-- | The response's http status code.
describeRouterConfigurationResponse_httpStatus :: Lens.Lens' DescribeRouterConfigurationResponse Prelude.Int
describeRouterConfigurationResponse_httpStatus = Lens.lens (\DescribeRouterConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeRouterConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeRouterConfigurationResponse)

instance
  Prelude.NFData
    DescribeRouterConfigurationResponse
  where
  rnf DescribeRouterConfigurationResponse' {..} =
    Prelude.rnf customerRouterConfig
      `Prelude.seq` Prelude.rnf router
      `Prelude.seq` Prelude.rnf virtualInterfaceId
      `Prelude.seq` Prelude.rnf virtualInterfaceName
      `Prelude.seq` Prelude.rnf httpStatus
