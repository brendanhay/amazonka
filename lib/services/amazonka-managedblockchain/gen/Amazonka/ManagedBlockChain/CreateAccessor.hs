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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new accessor for use with Managed Blockchain Ethereum nodes.
-- An accessor contains information required for token based access to your
-- Ethereum nodes.
module Amazonka.ManagedBlockChain.CreateAccessor
  ( -- * Creating a Request
    CreateAccessor (..),
    newCreateAccessor,

    -- * Request Lenses
    createAccessor_tags,
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
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAccessor' smart constructor.
data CreateAccessor = CreateAccessor'
  { -- | Tags to assign to the Accessor.
    --
    -- Each tag consists of a key and an optional value. You can specify
    -- multiple key-value pairs in a single request with an overall maximum of
    -- 50 tags allowed per resource.
    --
    -- For more information about tags, see
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
    -- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
    -- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | This is a unique, case-sensitive identifier that you provide to ensure
    -- the idempotency of the operation. An idempotent operation completes no
    -- more than once. This identifier is required only if you make a service
    -- request directly using an HTTP client. It is generated automatically if
    -- you use an Amazon Web Services SDK or the Amazon Web Services CLI.
    clientRequestToken :: Prelude.Text,
    -- | The type of accessor.
    --
    -- Currently, accessor type is restricted to @BILLING_TOKEN@.
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
-- 'tags', 'createAccessor_tags' - Tags to assign to the Accessor.
--
-- Each tag consists of a key and an optional value. You can specify
-- multiple key-value pairs in a single request with an overall maximum of
-- 50 tags allowed per resource.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
--
-- 'clientRequestToken', 'createAccessor_clientRequestToken' - This is a unique, case-sensitive identifier that you provide to ensure
-- the idempotency of the operation. An idempotent operation completes no
-- more than once. This identifier is required only if you make a service
-- request directly using an HTTP client. It is generated automatically if
-- you use an Amazon Web Services SDK or the Amazon Web Services CLI.
--
-- 'accessorType', 'createAccessor_accessorType' - The type of accessor.
--
-- Currently, accessor type is restricted to @BILLING_TOKEN@.
newCreateAccessor ::
  -- | 'clientRequestToken'
  Prelude.Text ->
  -- | 'accessorType'
  AccessorType ->
  CreateAccessor
newCreateAccessor pClientRequestToken_ pAccessorType_ =
  CreateAccessor'
    { tags = Prelude.Nothing,
      clientRequestToken = pClientRequestToken_,
      accessorType = pAccessorType_
    }

-- | Tags to assign to the Accessor.
--
-- Each tag consists of a key and an optional value. You can specify
-- multiple key-value pairs in a single request with an overall maximum of
-- 50 tags allowed per resource.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
createAccessor_tags :: Lens.Lens' CreateAccessor (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAccessor_tags = Lens.lens (\CreateAccessor' {tags} -> tags) (\s@CreateAccessor' {} a -> s {tags = a} :: CreateAccessor) Prelude.. Lens.mapping Lens.coerced

-- | This is a unique, case-sensitive identifier that you provide to ensure
-- the idempotency of the operation. An idempotent operation completes no
-- more than once. This identifier is required only if you make a service
-- request directly using an HTTP client. It is generated automatically if
-- you use an Amazon Web Services SDK or the Amazon Web Services CLI.
createAccessor_clientRequestToken :: Lens.Lens' CreateAccessor Prelude.Text
createAccessor_clientRequestToken = Lens.lens (\CreateAccessor' {clientRequestToken} -> clientRequestToken) (\s@CreateAccessor' {} a -> s {clientRequestToken = a} :: CreateAccessor)

-- | The type of accessor.
--
-- Currently, accessor type is restricted to @BILLING_TOKEN@.
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
            Prelude.<$> (x Data..?> "AccessorId")
            Prelude.<*> (x Data..?> "BillingToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAccessor where
  hashWithSalt _salt CreateAccessor' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` accessorType

instance Prelude.NFData CreateAccessor where
  rnf CreateAccessor' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf accessorType

instance Data.ToHeaders CreateAccessor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAccessor where
  toJSON CreateAccessor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken),
            Prelude.Just ("AccessorType" Data..= accessorType)
          ]
      )

instance Data.ToPath CreateAccessor where
  toPath = Prelude.const "/accessors"

instance Data.ToQuery CreateAccessor where
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
