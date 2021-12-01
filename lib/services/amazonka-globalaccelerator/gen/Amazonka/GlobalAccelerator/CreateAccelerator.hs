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
-- Module      : Amazonka.GlobalAccelerator.CreateAccelerator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an accelerator. An accelerator includes one or more listeners
-- that process inbound connections and direct traffic to one or more
-- endpoint groups, each of which includes endpoints, such as Network Load
-- Balancers.
--
-- Global Accelerator is a global service that supports endpoints in
-- multiple AWS Regions but you must specify the US West (Oregon) Region to
-- create or update accelerators.
module Amazonka.GlobalAccelerator.CreateAccelerator
  ( -- * Creating a Request
    CreateAccelerator (..),
    newCreateAccelerator,

    -- * Request Lenses
    createAccelerator_enabled,
    createAccelerator_ipAddressType,
    createAccelerator_tags,
    createAccelerator_ipAddresses,
    createAccelerator_name,
    createAccelerator_idempotencyToken,

    -- * Destructuring the Response
    CreateAcceleratorResponse (..),
    newCreateAcceleratorResponse,

    -- * Response Lenses
    createAcceleratorResponse_accelerator,
    createAcceleratorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAccelerator' smart constructor.
data CreateAccelerator = CreateAccelerator'
  { -- | Indicates whether an accelerator is enabled. The value is true or false.
    -- The default value is true.
    --
    -- If the value is set to true, an accelerator cannot be deleted. If set to
    -- false, the accelerator can be deleted.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The value for the address type must be IPv4.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | Create tags for an accelerator.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/tagging-in-global-accelerator.html Tagging in AWS Global Accelerator>
    -- in the /AWS Global Accelerator Developer Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | Optionally, if you\'ve added your own IP address pool to Global
    -- Accelerator (BYOIP), you can choose IP addresses from your own pool to
    -- use for the accelerator\'s static IP addresses when you create an
    -- accelerator. You can specify one or two addresses, separated by a space.
    -- Do not include the \/32 suffix.
    --
    -- Only one IP address from each of your IP address ranges can be used for
    -- each accelerator. If you specify only one IP address from your IP
    -- address range, Global Accelerator assigns a second static IP address for
    -- the accelerator from the AWS IP address pool.
    --
    -- Note that you can\'t update IP addresses for an existing accelerator. To
    -- change them, you must create a new accelerator with the new addresses.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/using-byoip.html Bring Your Own IP Addresses (BYOIP)>
    -- in the /AWS Global Accelerator Developer Guide/.
    ipAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The name of an accelerator. The name can have a maximum of 32
    -- characters, must contain only alphanumeric characters or hyphens (-),
    -- and must not begin or end with a hyphen.
    name :: Prelude.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency—that is, the uniqueness—of an accelerator.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccelerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'createAccelerator_enabled' - Indicates whether an accelerator is enabled. The value is true or false.
-- The default value is true.
--
-- If the value is set to true, an accelerator cannot be deleted. If set to
-- false, the accelerator can be deleted.
--
-- 'ipAddressType', 'createAccelerator_ipAddressType' - The value for the address type must be IPv4.
--
-- 'tags', 'createAccelerator_tags' - Create tags for an accelerator.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/tagging-in-global-accelerator.html Tagging in AWS Global Accelerator>
-- in the /AWS Global Accelerator Developer Guide/.
--
-- 'ipAddresses', 'createAccelerator_ipAddresses' - Optionally, if you\'ve added your own IP address pool to Global
-- Accelerator (BYOIP), you can choose IP addresses from your own pool to
-- use for the accelerator\'s static IP addresses when you create an
-- accelerator. You can specify one or two addresses, separated by a space.
-- Do not include the \/32 suffix.
--
-- Only one IP address from each of your IP address ranges can be used for
-- each accelerator. If you specify only one IP address from your IP
-- address range, Global Accelerator assigns a second static IP address for
-- the accelerator from the AWS IP address pool.
--
-- Note that you can\'t update IP addresses for an existing accelerator. To
-- change them, you must create a new accelerator with the new addresses.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/using-byoip.html Bring Your Own IP Addresses (BYOIP)>
-- in the /AWS Global Accelerator Developer Guide/.
--
-- 'name', 'createAccelerator_name' - The name of an accelerator. The name can have a maximum of 32
-- characters, must contain only alphanumeric characters or hyphens (-),
-- and must not begin or end with a hyphen.
--
-- 'idempotencyToken', 'createAccelerator_idempotencyToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency—that is, the uniqueness—of an accelerator.
newCreateAccelerator ::
  -- | 'name'
  Prelude.Text ->
  -- | 'idempotencyToken'
  Prelude.Text ->
  CreateAccelerator
newCreateAccelerator pName_ pIdempotencyToken_ =
  CreateAccelerator'
    { enabled = Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      tags = Prelude.Nothing,
      ipAddresses = Prelude.Nothing,
      name = pName_,
      idempotencyToken = pIdempotencyToken_
    }

-- | Indicates whether an accelerator is enabled. The value is true or false.
-- The default value is true.
--
-- If the value is set to true, an accelerator cannot be deleted. If set to
-- false, the accelerator can be deleted.
createAccelerator_enabled :: Lens.Lens' CreateAccelerator (Prelude.Maybe Prelude.Bool)
createAccelerator_enabled = Lens.lens (\CreateAccelerator' {enabled} -> enabled) (\s@CreateAccelerator' {} a -> s {enabled = a} :: CreateAccelerator)

-- | The value for the address type must be IPv4.
createAccelerator_ipAddressType :: Lens.Lens' CreateAccelerator (Prelude.Maybe IpAddressType)
createAccelerator_ipAddressType = Lens.lens (\CreateAccelerator' {ipAddressType} -> ipAddressType) (\s@CreateAccelerator' {} a -> s {ipAddressType = a} :: CreateAccelerator)

-- | Create tags for an accelerator.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/tagging-in-global-accelerator.html Tagging in AWS Global Accelerator>
-- in the /AWS Global Accelerator Developer Guide/.
createAccelerator_tags :: Lens.Lens' CreateAccelerator (Prelude.Maybe [Tag])
createAccelerator_tags = Lens.lens (\CreateAccelerator' {tags} -> tags) (\s@CreateAccelerator' {} a -> s {tags = a} :: CreateAccelerator) Prelude.. Lens.mapping Lens.coerced

-- | Optionally, if you\'ve added your own IP address pool to Global
-- Accelerator (BYOIP), you can choose IP addresses from your own pool to
-- use for the accelerator\'s static IP addresses when you create an
-- accelerator. You can specify one or two addresses, separated by a space.
-- Do not include the \/32 suffix.
--
-- Only one IP address from each of your IP address ranges can be used for
-- each accelerator. If you specify only one IP address from your IP
-- address range, Global Accelerator assigns a second static IP address for
-- the accelerator from the AWS IP address pool.
--
-- Note that you can\'t update IP addresses for an existing accelerator. To
-- change them, you must create a new accelerator with the new addresses.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/using-byoip.html Bring Your Own IP Addresses (BYOIP)>
-- in the /AWS Global Accelerator Developer Guide/.
createAccelerator_ipAddresses :: Lens.Lens' CreateAccelerator (Prelude.Maybe [Prelude.Text])
createAccelerator_ipAddresses = Lens.lens (\CreateAccelerator' {ipAddresses} -> ipAddresses) (\s@CreateAccelerator' {} a -> s {ipAddresses = a} :: CreateAccelerator) Prelude.. Lens.mapping Lens.coerced

-- | The name of an accelerator. The name can have a maximum of 32
-- characters, must contain only alphanumeric characters or hyphens (-),
-- and must not begin or end with a hyphen.
createAccelerator_name :: Lens.Lens' CreateAccelerator Prelude.Text
createAccelerator_name = Lens.lens (\CreateAccelerator' {name} -> name) (\s@CreateAccelerator' {} a -> s {name = a} :: CreateAccelerator)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency—that is, the uniqueness—of an accelerator.
createAccelerator_idempotencyToken :: Lens.Lens' CreateAccelerator Prelude.Text
createAccelerator_idempotencyToken = Lens.lens (\CreateAccelerator' {idempotencyToken} -> idempotencyToken) (\s@CreateAccelerator' {} a -> s {idempotencyToken = a} :: CreateAccelerator)

instance Core.AWSRequest CreateAccelerator where
  type
    AWSResponse CreateAccelerator =
      CreateAcceleratorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAcceleratorResponse'
            Prelude.<$> (x Core..?> "Accelerator")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAccelerator where
  hashWithSalt salt' CreateAccelerator' {..} =
    salt' `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ipAddresses
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` ipAddressType
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData CreateAccelerator where
  rnf CreateAccelerator' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf idempotencyToken
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ipAddresses
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf ipAddressType

instance Core.ToHeaders CreateAccelerator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GlobalAccelerator_V20180706.CreateAccelerator" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateAccelerator where
  toJSON CreateAccelerator' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Enabled" Core..=) Prelude.<$> enabled,
            ("IpAddressType" Core..=) Prelude.<$> ipAddressType,
            ("Tags" Core..=) Prelude.<$> tags,
            ("IpAddresses" Core..=) Prelude.<$> ipAddresses,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just
              ("IdempotencyToken" Core..= idempotencyToken)
          ]
      )

instance Core.ToPath CreateAccelerator where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateAccelerator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAcceleratorResponse' smart constructor.
data CreateAcceleratorResponse = CreateAcceleratorResponse'
  { -- | The accelerator that is created by specifying a listener and the
    -- supported IP address types.
    accelerator :: Prelude.Maybe Accelerator,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAcceleratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accelerator', 'createAcceleratorResponse_accelerator' - The accelerator that is created by specifying a listener and the
-- supported IP address types.
--
-- 'httpStatus', 'createAcceleratorResponse_httpStatus' - The response's http status code.
newCreateAcceleratorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAcceleratorResponse
newCreateAcceleratorResponse pHttpStatus_ =
  CreateAcceleratorResponse'
    { accelerator =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The accelerator that is created by specifying a listener and the
-- supported IP address types.
createAcceleratorResponse_accelerator :: Lens.Lens' CreateAcceleratorResponse (Prelude.Maybe Accelerator)
createAcceleratorResponse_accelerator = Lens.lens (\CreateAcceleratorResponse' {accelerator} -> accelerator) (\s@CreateAcceleratorResponse' {} a -> s {accelerator = a} :: CreateAcceleratorResponse)

-- | The response's http status code.
createAcceleratorResponse_httpStatus :: Lens.Lens' CreateAcceleratorResponse Prelude.Int
createAcceleratorResponse_httpStatus = Lens.lens (\CreateAcceleratorResponse' {httpStatus} -> httpStatus) (\s@CreateAcceleratorResponse' {} a -> s {httpStatus = a} :: CreateAcceleratorResponse)

instance Prelude.NFData CreateAcceleratorResponse where
  rnf CreateAcceleratorResponse' {..} =
    Prelude.rnf accelerator
      `Prelude.seq` Prelude.rnf httpStatus
