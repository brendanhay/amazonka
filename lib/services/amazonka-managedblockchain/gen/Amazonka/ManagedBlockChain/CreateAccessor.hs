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
-- Module      : Amazonka.ManagedBlockChain.CreateAccessor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The token based access feature is in preview release for Ethereum on
-- Amazon Managed Blockchain and is subject to change. We recommend that
-- you use this feature only with test scenarios, and not in production
-- environments.
--
-- Creates a new accessor for use with Managed Blockchain Ethereum nodes.
-- An accessor object is a container that has the information required for
-- token based access to your Ethereum nodes.
module Amazonka.ManagedBlockChain.CreateAccessor
  ( -- * Creating a Request
    CreateAccessor (..),
    newCreateAccessor,

    -- * Request Lenses
    createAccessor_clientRequestToken,
    createAccessor_accessorType,

    -- * Destructuring the Response
    CreateAccessorResponse (..),
    newCreateAccessorResponse,

    -- * Response Lenses
    createAccessorResponse_accessorId,
    createAccessorResponse_billingToken,
    createAccessorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAccessor' smart constructor.
data CreateAccessor = CreateAccessor'
  { -- | This is a unique, case-sensitive identifier that you provide to ensure
    -- the idempotency of the operation. An idempotent operation completes no
    -- more than once. This identifier is required only if you make a service
    -- request directly using an HTTP client. It is generated automatically if
    -- you use an Amazon Web Services SDK or the Amazon Web Services CLI.
    clientRequestToken :: Prelude.Text,
    -- | The type of accessor.
    --
    -- Currently accessor type is restricted to @BILLING_TOKEN@.
    accessorType :: AccessorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccessor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createAccessor_clientRequestToken' - This is a unique, case-sensitive identifier that you provide to ensure
-- the idempotency of the operation. An idempotent operation completes no
-- more than once. This identifier is required only if you make a service
-- request directly using an HTTP client. It is generated automatically if
-- you use an Amazon Web Services SDK or the Amazon Web Services CLI.
--
-- 'accessorType', 'createAccessor_accessorType' - The type of accessor.
--
-- Currently accessor type is restricted to @BILLING_TOKEN@.
newCreateAccessor ::
  -- | 'clientRequestToken'
  Prelude.Text ->
  -- | 'accessorType'
  AccessorType ->
  CreateAccessor
newCreateAccessor pClientRequestToken_ pAccessorType_ =
  CreateAccessor'
    { clientRequestToken =
        pClientRequestToken_,
      accessorType = pAccessorType_
    }

-- | This is a unique, case-sensitive identifier that you provide to ensure
-- the idempotency of the operation. An idempotent operation completes no
-- more than once. This identifier is required only if you make a service
-- request directly using an HTTP client. It is generated automatically if
-- you use an Amazon Web Services SDK or the Amazon Web Services CLI.
createAccessor_clientRequestToken :: Lens.Lens' CreateAccessor Prelude.Text
createAccessor_clientRequestToken = Lens.lens (\CreateAccessor' {clientRequestToken} -> clientRequestToken) (\s@CreateAccessor' {} a -> s {clientRequestToken = a} :: CreateAccessor)

-- | The type of accessor.
--
-- Currently accessor type is restricted to @BILLING_TOKEN@.
createAccessor_accessorType :: Lens.Lens' CreateAccessor AccessorType
createAccessor_accessorType = Lens.lens (\CreateAccessor' {accessorType} -> accessorType) (\s@CreateAccessor' {} a -> s {accessorType = a} :: CreateAccessor)

instance Core.AWSRequest CreateAccessor where
  type
    AWSResponse CreateAccessor =
      CreateAccessorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAccessorResponse'
            Prelude.<$> (x Core..?> "AccessorId")
            Prelude.<*> (x Core..?> "BillingToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAccessor where
  hashWithSalt _salt CreateAccessor' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` accessorType

instance Prelude.NFData CreateAccessor where
  rnf CreateAccessor' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf accessorType

instance Core.ToHeaders CreateAccessor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateAccessor where
  toJSON CreateAccessor' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ClientRequestToken" Core..= clientRequestToken),
            Prelude.Just ("AccessorType" Core..= accessorType)
          ]
      )

instance Core.ToPath CreateAccessor where
  toPath = Prelude.const "/accessors"

instance Core.ToQuery CreateAccessor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAccessorResponse' smart constructor.
data CreateAccessorResponse = CreateAccessorResponse'
  { -- | The unique identifier of the accessor.
    accessorId :: Prelude.Maybe Prelude.Text,
    -- | The billing token is a property of the Accessor. Use this token to make
    -- Ethereum API calls to your Ethereum node. The billing token is used to
    -- track your accessor object for billing Ethereum API requests made to
    -- your Ethereum nodes.
    billingToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccessorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessorId', 'createAccessorResponse_accessorId' - The unique identifier of the accessor.
--
-- 'billingToken', 'createAccessorResponse_billingToken' - The billing token is a property of the Accessor. Use this token to make
-- Ethereum API calls to your Ethereum node. The billing token is used to
-- track your accessor object for billing Ethereum API requests made to
-- your Ethereum nodes.
--
-- 'httpStatus', 'createAccessorResponse_httpStatus' - The response's http status code.
newCreateAccessorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAccessorResponse
newCreateAccessorResponse pHttpStatus_ =
  CreateAccessorResponse'
    { accessorId =
        Prelude.Nothing,
      billingToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the accessor.
createAccessorResponse_accessorId :: Lens.Lens' CreateAccessorResponse (Prelude.Maybe Prelude.Text)
createAccessorResponse_accessorId = Lens.lens (\CreateAccessorResponse' {accessorId} -> accessorId) (\s@CreateAccessorResponse' {} a -> s {accessorId = a} :: CreateAccessorResponse)

-- | The billing token is a property of the Accessor. Use this token to make
-- Ethereum API calls to your Ethereum node. The billing token is used to
-- track your accessor object for billing Ethereum API requests made to
-- your Ethereum nodes.
createAccessorResponse_billingToken :: Lens.Lens' CreateAccessorResponse (Prelude.Maybe Prelude.Text)
createAccessorResponse_billingToken = Lens.lens (\CreateAccessorResponse' {billingToken} -> billingToken) (\s@CreateAccessorResponse' {} a -> s {billingToken = a} :: CreateAccessorResponse)

-- | The response's http status code.
createAccessorResponse_httpStatus :: Lens.Lens' CreateAccessorResponse Prelude.Int
createAccessorResponse_httpStatus = Lens.lens (\CreateAccessorResponse' {httpStatus} -> httpStatus) (\s@CreateAccessorResponse' {} a -> s {httpStatus = a} :: CreateAccessorResponse)

instance Prelude.NFData CreateAccessorResponse where
  rnf CreateAccessorResponse' {..} =
    Prelude.rnf accessorId
      `Prelude.seq` Prelude.rnf billingToken
      `Prelude.seq` Prelude.rnf httpStatus
