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
-- Module      : Amazonka.GlobalAccelerator.UpdateCustomRoutingAccelerator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a custom routing accelerator.
module Amazonka.GlobalAccelerator.UpdateCustomRoutingAccelerator
  ( -- * Creating a Request
    UpdateCustomRoutingAccelerator (..),
    newUpdateCustomRoutingAccelerator,

    -- * Request Lenses
    updateCustomRoutingAccelerator_name,
    updateCustomRoutingAccelerator_enabled,
    updateCustomRoutingAccelerator_ipAddressType,
    updateCustomRoutingAccelerator_acceleratorArn,

    -- * Destructuring the Response
    UpdateCustomRoutingAcceleratorResponse (..),
    newUpdateCustomRoutingAcceleratorResponse,

    -- * Response Lenses
    updateCustomRoutingAcceleratorResponse_accelerator,
    updateCustomRoutingAcceleratorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCustomRoutingAccelerator' smart constructor.
data UpdateCustomRoutingAccelerator = UpdateCustomRoutingAccelerator'
  { -- | The name of the accelerator. The name can have a maximum of 64
    -- characters, must contain only alphanumeric characters, periods (.), or
    -- hyphens (-), and must not begin or end with a hyphen or period.
    name :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether an accelerator is enabled. The value is true or false.
    -- The default value is true.
    --
    -- If the value is set to true, the accelerator cannot be deleted. If set
    -- to false, the accelerator can be deleted.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The IP address type that an accelerator supports. For a custom routing
    -- accelerator, the value must be IPV4.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | The Amazon Resource Name (ARN) of the accelerator to update.
    acceleratorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCustomRoutingAccelerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateCustomRoutingAccelerator_name' - The name of the accelerator. The name can have a maximum of 64
-- characters, must contain only alphanumeric characters, periods (.), or
-- hyphens (-), and must not begin or end with a hyphen or period.
--
-- 'enabled', 'updateCustomRoutingAccelerator_enabled' - Indicates whether an accelerator is enabled. The value is true or false.
-- The default value is true.
--
-- If the value is set to true, the accelerator cannot be deleted. If set
-- to false, the accelerator can be deleted.
--
-- 'ipAddressType', 'updateCustomRoutingAccelerator_ipAddressType' - The IP address type that an accelerator supports. For a custom routing
-- accelerator, the value must be IPV4.
--
-- 'acceleratorArn', 'updateCustomRoutingAccelerator_acceleratorArn' - The Amazon Resource Name (ARN) of the accelerator to update.
newUpdateCustomRoutingAccelerator ::
  -- | 'acceleratorArn'
  Prelude.Text ->
  UpdateCustomRoutingAccelerator
newUpdateCustomRoutingAccelerator pAcceleratorArn_ =
  UpdateCustomRoutingAccelerator'
    { name =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      acceleratorArn = pAcceleratorArn_
    }

-- | The name of the accelerator. The name can have a maximum of 64
-- characters, must contain only alphanumeric characters, periods (.), or
-- hyphens (-), and must not begin or end with a hyphen or period.
updateCustomRoutingAccelerator_name :: Lens.Lens' UpdateCustomRoutingAccelerator (Prelude.Maybe Prelude.Text)
updateCustomRoutingAccelerator_name = Lens.lens (\UpdateCustomRoutingAccelerator' {name} -> name) (\s@UpdateCustomRoutingAccelerator' {} a -> s {name = a} :: UpdateCustomRoutingAccelerator)

-- | Indicates whether an accelerator is enabled. The value is true or false.
-- The default value is true.
--
-- If the value is set to true, the accelerator cannot be deleted. If set
-- to false, the accelerator can be deleted.
updateCustomRoutingAccelerator_enabled :: Lens.Lens' UpdateCustomRoutingAccelerator (Prelude.Maybe Prelude.Bool)
updateCustomRoutingAccelerator_enabled = Lens.lens (\UpdateCustomRoutingAccelerator' {enabled} -> enabled) (\s@UpdateCustomRoutingAccelerator' {} a -> s {enabled = a} :: UpdateCustomRoutingAccelerator)

-- | The IP address type that an accelerator supports. For a custom routing
-- accelerator, the value must be IPV4.
updateCustomRoutingAccelerator_ipAddressType :: Lens.Lens' UpdateCustomRoutingAccelerator (Prelude.Maybe IpAddressType)
updateCustomRoutingAccelerator_ipAddressType = Lens.lens (\UpdateCustomRoutingAccelerator' {ipAddressType} -> ipAddressType) (\s@UpdateCustomRoutingAccelerator' {} a -> s {ipAddressType = a} :: UpdateCustomRoutingAccelerator)

-- | The Amazon Resource Name (ARN) of the accelerator to update.
updateCustomRoutingAccelerator_acceleratorArn :: Lens.Lens' UpdateCustomRoutingAccelerator Prelude.Text
updateCustomRoutingAccelerator_acceleratorArn = Lens.lens (\UpdateCustomRoutingAccelerator' {acceleratorArn} -> acceleratorArn) (\s@UpdateCustomRoutingAccelerator' {} a -> s {acceleratorArn = a} :: UpdateCustomRoutingAccelerator)

instance
  Core.AWSRequest
    UpdateCustomRoutingAccelerator
  where
  type
    AWSResponse UpdateCustomRoutingAccelerator =
      UpdateCustomRoutingAcceleratorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCustomRoutingAcceleratorResponse'
            Prelude.<$> (x Core..?> "Accelerator")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateCustomRoutingAccelerator
  where
  hashWithSalt
    _salt
    UpdateCustomRoutingAccelerator' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` enabled
        `Prelude.hashWithSalt` ipAddressType
        `Prelude.hashWithSalt` acceleratorArn

instance
  Prelude.NFData
    UpdateCustomRoutingAccelerator
  where
  rnf UpdateCustomRoutingAccelerator' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf ipAddressType
      `Prelude.seq` Prelude.rnf acceleratorArn

instance
  Core.ToHeaders
    UpdateCustomRoutingAccelerator
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GlobalAccelerator_V20180706.UpdateCustomRoutingAccelerator" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateCustomRoutingAccelerator where
  toJSON UpdateCustomRoutingAccelerator' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("Enabled" Core..=) Prelude.<$> enabled,
            ("IpAddressType" Core..=) Prelude.<$> ipAddressType,
            Prelude.Just
              ("AcceleratorArn" Core..= acceleratorArn)
          ]
      )

instance Core.ToPath UpdateCustomRoutingAccelerator where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateCustomRoutingAccelerator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCustomRoutingAcceleratorResponse' smart constructor.
data UpdateCustomRoutingAcceleratorResponse = UpdateCustomRoutingAcceleratorResponse'
  { -- | Information about the updated custom routing accelerator.
    accelerator :: Prelude.Maybe CustomRoutingAccelerator,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCustomRoutingAcceleratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accelerator', 'updateCustomRoutingAcceleratorResponse_accelerator' - Information about the updated custom routing accelerator.
--
-- 'httpStatus', 'updateCustomRoutingAcceleratorResponse_httpStatus' - The response's http status code.
newUpdateCustomRoutingAcceleratorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCustomRoutingAcceleratorResponse
newUpdateCustomRoutingAcceleratorResponse
  pHttpStatus_ =
    UpdateCustomRoutingAcceleratorResponse'
      { accelerator =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the updated custom routing accelerator.
updateCustomRoutingAcceleratorResponse_accelerator :: Lens.Lens' UpdateCustomRoutingAcceleratorResponse (Prelude.Maybe CustomRoutingAccelerator)
updateCustomRoutingAcceleratorResponse_accelerator = Lens.lens (\UpdateCustomRoutingAcceleratorResponse' {accelerator} -> accelerator) (\s@UpdateCustomRoutingAcceleratorResponse' {} a -> s {accelerator = a} :: UpdateCustomRoutingAcceleratorResponse)

-- | The response's http status code.
updateCustomRoutingAcceleratorResponse_httpStatus :: Lens.Lens' UpdateCustomRoutingAcceleratorResponse Prelude.Int
updateCustomRoutingAcceleratorResponse_httpStatus = Lens.lens (\UpdateCustomRoutingAcceleratorResponse' {httpStatus} -> httpStatus) (\s@UpdateCustomRoutingAcceleratorResponse' {} a -> s {httpStatus = a} :: UpdateCustomRoutingAcceleratorResponse)

instance
  Prelude.NFData
    UpdateCustomRoutingAcceleratorResponse
  where
  rnf UpdateCustomRoutingAcceleratorResponse' {..} =
    Prelude.rnf accelerator
      `Prelude.seq` Prelude.rnf httpStatus
