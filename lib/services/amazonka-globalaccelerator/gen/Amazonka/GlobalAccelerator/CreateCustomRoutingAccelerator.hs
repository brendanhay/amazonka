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
-- Module      : Amazonka.GlobalAccelerator.CreateCustomRoutingAccelerator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a custom routing accelerator. A custom routing accelerator
-- directs traffic to one of possibly thousands of Amazon EC2 instance
-- destinations running in a single or multiple virtual private clouds
-- (VPC) subnet endpoints.
--
-- Be aware that, by default, all destination EC2 instances in a VPC subnet
-- endpoint cannot receive traffic. To enable all destinations to receive
-- traffic, or to specify individual port mappings that can receive
-- traffic, see the
-- <https://docs.aws.amazon.com/global-accelerator/latest/api/API_AllowCustomRoutingTraffic.html AllowCustomRoutingTraffic>
-- operation.
--
-- Global Accelerator is a global service that supports endpoints in
-- multiple Amazon Web Services Regions but you must specify the US West
-- (Oregon) Region to create, update, or otherwise work with accelerators.
-- That is, for example, specify @--region us-west-2@ on AWS CLI commands.
module Amazonka.GlobalAccelerator.CreateCustomRoutingAccelerator
  ( -- * Creating a Request
    CreateCustomRoutingAccelerator (..),
    newCreateCustomRoutingAccelerator,

    -- * Request Lenses
    createCustomRoutingAccelerator_enabled,
    createCustomRoutingAccelerator_ipAddressType,
    createCustomRoutingAccelerator_ipAddresses,
    createCustomRoutingAccelerator_tags,
    createCustomRoutingAccelerator_name,
    createCustomRoutingAccelerator_idempotencyToken,

    -- * Destructuring the Response
    CreateCustomRoutingAcceleratorResponse (..),
    newCreateCustomRoutingAcceleratorResponse,

    -- * Response Lenses
    createCustomRoutingAcceleratorResponse_accelerator,
    createCustomRoutingAcceleratorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCustomRoutingAccelerator' smart constructor.
data CreateCustomRoutingAccelerator = CreateCustomRoutingAccelerator'
  { -- | Indicates whether an accelerator is enabled. The value is true or false.
    -- The default value is true.
    --
    -- If the value is set to true, an accelerator cannot be deleted. If set to
    -- false, the accelerator can be deleted.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The IP address type that an accelerator supports. For a custom routing
    -- accelerator, the value must be IPV4.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | Optionally, if you\'ve added your own IP address pool to Global
    -- Accelerator (BYOIP), you can choose an IPv4 address from your own pool
    -- to use for the accelerator\'s static IPv4 address when you create an
    -- accelerator.
    --
    -- After you bring an address range to Amazon Web Services, it appears in
    -- your account as an address pool. When you create an accelerator, you can
    -- assign one IPv4 address from your range to it. Global Accelerator
    -- assigns you a second static IPv4 address from an Amazon IP address
    -- range. If you bring two IPv4 address ranges to Amazon Web Services, you
    -- can assign one IPv4 address from each range to your accelerator. This
    -- restriction is because Global Accelerator assigns each address range to
    -- a different network zone, for high availability.
    --
    -- You can specify one or two addresses, separated by a space. Do not
    -- include the \/32 suffix.
    --
    -- Note that you can\'t update IP addresses for an existing accelerator. To
    -- change them, you must create a new accelerator with the new addresses.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/using-byoip.html Bring your own IP addresses (BYOIP)>
    -- in the /Global Accelerator Developer Guide/.
    ipAddresses :: Prelude.Maybe [Prelude.Text],
    -- | Create tags for an accelerator.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/tagging-in-global-accelerator.html Tagging in Global Accelerator>
    -- in the /Global Accelerator Developer Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The name of a custom routing accelerator. The name can have a maximum of
    -- 64 characters, must contain only alphanumeric characters or hyphens (-),
    -- and must not begin or end with a hyphen.
    name :: Prelude.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency—that is, the uniqueness—of the request.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomRoutingAccelerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'createCustomRoutingAccelerator_enabled' - Indicates whether an accelerator is enabled. The value is true or false.
-- The default value is true.
--
-- If the value is set to true, an accelerator cannot be deleted. If set to
-- false, the accelerator can be deleted.
--
-- 'ipAddressType', 'createCustomRoutingAccelerator_ipAddressType' - The IP address type that an accelerator supports. For a custom routing
-- accelerator, the value must be IPV4.
--
-- 'ipAddresses', 'createCustomRoutingAccelerator_ipAddresses' - Optionally, if you\'ve added your own IP address pool to Global
-- Accelerator (BYOIP), you can choose an IPv4 address from your own pool
-- to use for the accelerator\'s static IPv4 address when you create an
-- accelerator.
--
-- After you bring an address range to Amazon Web Services, it appears in
-- your account as an address pool. When you create an accelerator, you can
-- assign one IPv4 address from your range to it. Global Accelerator
-- assigns you a second static IPv4 address from an Amazon IP address
-- range. If you bring two IPv4 address ranges to Amazon Web Services, you
-- can assign one IPv4 address from each range to your accelerator. This
-- restriction is because Global Accelerator assigns each address range to
-- a different network zone, for high availability.
--
-- You can specify one or two addresses, separated by a space. Do not
-- include the \/32 suffix.
--
-- Note that you can\'t update IP addresses for an existing accelerator. To
-- change them, you must create a new accelerator with the new addresses.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/using-byoip.html Bring your own IP addresses (BYOIP)>
-- in the /Global Accelerator Developer Guide/.
--
-- 'tags', 'createCustomRoutingAccelerator_tags' - Create tags for an accelerator.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/tagging-in-global-accelerator.html Tagging in Global Accelerator>
-- in the /Global Accelerator Developer Guide/.
--
-- 'name', 'createCustomRoutingAccelerator_name' - The name of a custom routing accelerator. The name can have a maximum of
-- 64 characters, must contain only alphanumeric characters or hyphens (-),
-- and must not begin or end with a hyphen.
--
-- 'idempotencyToken', 'createCustomRoutingAccelerator_idempotencyToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency—that is, the uniqueness—of the request.
newCreateCustomRoutingAccelerator ::
  -- | 'name'
  Prelude.Text ->
  -- | 'idempotencyToken'
  Prelude.Text ->
  CreateCustomRoutingAccelerator
newCreateCustomRoutingAccelerator
  pName_
  pIdempotencyToken_ =
    CreateCustomRoutingAccelerator'
      { enabled =
          Prelude.Nothing,
        ipAddressType = Prelude.Nothing,
        ipAddresses = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        idempotencyToken = pIdempotencyToken_
      }

-- | Indicates whether an accelerator is enabled. The value is true or false.
-- The default value is true.
--
-- If the value is set to true, an accelerator cannot be deleted. If set to
-- false, the accelerator can be deleted.
createCustomRoutingAccelerator_enabled :: Lens.Lens' CreateCustomRoutingAccelerator (Prelude.Maybe Prelude.Bool)
createCustomRoutingAccelerator_enabled = Lens.lens (\CreateCustomRoutingAccelerator' {enabled} -> enabled) (\s@CreateCustomRoutingAccelerator' {} a -> s {enabled = a} :: CreateCustomRoutingAccelerator)

-- | The IP address type that an accelerator supports. For a custom routing
-- accelerator, the value must be IPV4.
createCustomRoutingAccelerator_ipAddressType :: Lens.Lens' CreateCustomRoutingAccelerator (Prelude.Maybe IpAddressType)
createCustomRoutingAccelerator_ipAddressType = Lens.lens (\CreateCustomRoutingAccelerator' {ipAddressType} -> ipAddressType) (\s@CreateCustomRoutingAccelerator' {} a -> s {ipAddressType = a} :: CreateCustomRoutingAccelerator)

-- | Optionally, if you\'ve added your own IP address pool to Global
-- Accelerator (BYOIP), you can choose an IPv4 address from your own pool
-- to use for the accelerator\'s static IPv4 address when you create an
-- accelerator.
--
-- After you bring an address range to Amazon Web Services, it appears in
-- your account as an address pool. When you create an accelerator, you can
-- assign one IPv4 address from your range to it. Global Accelerator
-- assigns you a second static IPv4 address from an Amazon IP address
-- range. If you bring two IPv4 address ranges to Amazon Web Services, you
-- can assign one IPv4 address from each range to your accelerator. This
-- restriction is because Global Accelerator assigns each address range to
-- a different network zone, for high availability.
--
-- You can specify one or two addresses, separated by a space. Do not
-- include the \/32 suffix.
--
-- Note that you can\'t update IP addresses for an existing accelerator. To
-- change them, you must create a new accelerator with the new addresses.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/using-byoip.html Bring your own IP addresses (BYOIP)>
-- in the /Global Accelerator Developer Guide/.
createCustomRoutingAccelerator_ipAddresses :: Lens.Lens' CreateCustomRoutingAccelerator (Prelude.Maybe [Prelude.Text])
createCustomRoutingAccelerator_ipAddresses = Lens.lens (\CreateCustomRoutingAccelerator' {ipAddresses} -> ipAddresses) (\s@CreateCustomRoutingAccelerator' {} a -> s {ipAddresses = a} :: CreateCustomRoutingAccelerator) Prelude.. Lens.mapping Lens.coerced

-- | Create tags for an accelerator.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/tagging-in-global-accelerator.html Tagging in Global Accelerator>
-- in the /Global Accelerator Developer Guide/.
createCustomRoutingAccelerator_tags :: Lens.Lens' CreateCustomRoutingAccelerator (Prelude.Maybe [Tag])
createCustomRoutingAccelerator_tags = Lens.lens (\CreateCustomRoutingAccelerator' {tags} -> tags) (\s@CreateCustomRoutingAccelerator' {} a -> s {tags = a} :: CreateCustomRoutingAccelerator) Prelude.. Lens.mapping Lens.coerced

-- | The name of a custom routing accelerator. The name can have a maximum of
-- 64 characters, must contain only alphanumeric characters or hyphens (-),
-- and must not begin or end with a hyphen.
createCustomRoutingAccelerator_name :: Lens.Lens' CreateCustomRoutingAccelerator Prelude.Text
createCustomRoutingAccelerator_name = Lens.lens (\CreateCustomRoutingAccelerator' {name} -> name) (\s@CreateCustomRoutingAccelerator' {} a -> s {name = a} :: CreateCustomRoutingAccelerator)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency—that is, the uniqueness—of the request.
createCustomRoutingAccelerator_idempotencyToken :: Lens.Lens' CreateCustomRoutingAccelerator Prelude.Text
createCustomRoutingAccelerator_idempotencyToken = Lens.lens (\CreateCustomRoutingAccelerator' {idempotencyToken} -> idempotencyToken) (\s@CreateCustomRoutingAccelerator' {} a -> s {idempotencyToken = a} :: CreateCustomRoutingAccelerator)

instance
  Core.AWSRequest
    CreateCustomRoutingAccelerator
  where
  type
    AWSResponse CreateCustomRoutingAccelerator =
      CreateCustomRoutingAcceleratorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCustomRoutingAcceleratorResponse'
            Prelude.<$> (x Data..?> "Accelerator")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateCustomRoutingAccelerator
  where
  hashWithSalt
    _salt
    CreateCustomRoutingAccelerator' {..} =
      _salt
        `Prelude.hashWithSalt` enabled
        `Prelude.hashWithSalt` ipAddressType
        `Prelude.hashWithSalt` ipAddresses
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` idempotencyToken

instance
  Prelude.NFData
    CreateCustomRoutingAccelerator
  where
  rnf CreateCustomRoutingAccelerator' {..} =
    Prelude.rnf enabled `Prelude.seq`
      Prelude.rnf ipAddressType `Prelude.seq`
        Prelude.rnf ipAddresses `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf idempotencyToken

instance
  Data.ToHeaders
    CreateCustomRoutingAccelerator
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.CreateCustomRoutingAccelerator" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCustomRoutingAccelerator where
  toJSON CreateCustomRoutingAccelerator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Enabled" Data..=) Prelude.<$> enabled,
            ("IpAddressType" Data..=) Prelude.<$> ipAddressType,
            ("IpAddresses" Data..=) Prelude.<$> ipAddresses,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("IdempotencyToken" Data..= idempotencyToken)
          ]
      )

instance Data.ToPath CreateCustomRoutingAccelerator where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCustomRoutingAccelerator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCustomRoutingAcceleratorResponse' smart constructor.
data CreateCustomRoutingAcceleratorResponse = CreateCustomRoutingAcceleratorResponse'
  { -- | The accelerator that is created.
    accelerator :: Prelude.Maybe CustomRoutingAccelerator,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomRoutingAcceleratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accelerator', 'createCustomRoutingAcceleratorResponse_accelerator' - The accelerator that is created.
--
-- 'httpStatus', 'createCustomRoutingAcceleratorResponse_httpStatus' - The response's http status code.
newCreateCustomRoutingAcceleratorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCustomRoutingAcceleratorResponse
newCreateCustomRoutingAcceleratorResponse
  pHttpStatus_ =
    CreateCustomRoutingAcceleratorResponse'
      { accelerator =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The accelerator that is created.
createCustomRoutingAcceleratorResponse_accelerator :: Lens.Lens' CreateCustomRoutingAcceleratorResponse (Prelude.Maybe CustomRoutingAccelerator)
createCustomRoutingAcceleratorResponse_accelerator = Lens.lens (\CreateCustomRoutingAcceleratorResponse' {accelerator} -> accelerator) (\s@CreateCustomRoutingAcceleratorResponse' {} a -> s {accelerator = a} :: CreateCustomRoutingAcceleratorResponse)

-- | The response's http status code.
createCustomRoutingAcceleratorResponse_httpStatus :: Lens.Lens' CreateCustomRoutingAcceleratorResponse Prelude.Int
createCustomRoutingAcceleratorResponse_httpStatus = Lens.lens (\CreateCustomRoutingAcceleratorResponse' {httpStatus} -> httpStatus) (\s@CreateCustomRoutingAcceleratorResponse' {} a -> s {httpStatus = a} :: CreateCustomRoutingAcceleratorResponse)

instance
  Prelude.NFData
    CreateCustomRoutingAcceleratorResponse
  where
  rnf CreateCustomRoutingAcceleratorResponse' {..} =
    Prelude.rnf accelerator `Prelude.seq`
      Prelude.rnf httpStatus
