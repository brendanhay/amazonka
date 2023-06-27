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
-- Module      : Amazonka.VPCLattice.CreateAccessLogSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables access logs to be sent to Amazon CloudWatch, Amazon S3, and
-- Amazon Kinesis Data Firehose. The service network owner can use the
-- access logs to audit the services in the network. The service network
-- owner will only see access logs from clients and services that are
-- associated with their service network. Access log entries represent
-- traffic originated from VPCs associated with that network. For more
-- information, see
-- <https://docs.aws.amazon.com/vpc-lattice/latest/ug/monitoring-access-logs.html Access logs>
-- in the /Amazon VPC Lattice User Guide/.
module Amazonka.VPCLattice.CreateAccessLogSubscription
  ( -- * Creating a Request
    CreateAccessLogSubscription (..),
    newCreateAccessLogSubscription,

    -- * Request Lenses
    createAccessLogSubscription_clientToken,
    createAccessLogSubscription_tags,
    createAccessLogSubscription_destinationArn,
    createAccessLogSubscription_resourceIdentifier,

    -- * Destructuring the Response
    CreateAccessLogSubscriptionResponse (..),
    newCreateAccessLogSubscriptionResponse,

    -- * Response Lenses
    createAccessLogSubscriptionResponse_httpStatus,
    createAccessLogSubscriptionResponse_arn,
    createAccessLogSubscriptionResponse_destinationArn,
    createAccessLogSubscriptionResponse_id,
    createAccessLogSubscriptionResponse_resourceArn,
    createAccessLogSubscriptionResponse_resourceId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newCreateAccessLogSubscription' smart constructor.
data CreateAccessLogSubscription = CreateAccessLogSubscription'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you retry a request that completed
    -- successfully using the same client token and parameters, the retry
    -- succeeds without performing any actions. If the parameters aren\'t
    -- identical, the retry fails.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The tags for the access log subscription.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the destination. The supported
    -- destination types are CloudWatch Log groups, Kinesis Data Firehose
    -- delivery streams, and Amazon S3 buckets.
    destinationArn :: Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the service network or service.
    resourceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccessLogSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createAccessLogSubscription_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you retry a request that completed
-- successfully using the same client token and parameters, the retry
-- succeeds without performing any actions. If the parameters aren\'t
-- identical, the retry fails.
--
-- 'tags', 'createAccessLogSubscription_tags' - The tags for the access log subscription.
--
-- 'destinationArn', 'createAccessLogSubscription_destinationArn' - The Amazon Resource Name (ARN) of the destination. The supported
-- destination types are CloudWatch Log groups, Kinesis Data Firehose
-- delivery streams, and Amazon S3 buckets.
--
-- 'resourceIdentifier', 'createAccessLogSubscription_resourceIdentifier' - The ID or Amazon Resource Name (ARN) of the service network or service.
newCreateAccessLogSubscription ::
  -- | 'destinationArn'
  Prelude.Text ->
  -- | 'resourceIdentifier'
  Prelude.Text ->
  CreateAccessLogSubscription
newCreateAccessLogSubscription
  pDestinationArn_
  pResourceIdentifier_ =
    CreateAccessLogSubscription'
      { clientToken =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        destinationArn = pDestinationArn_,
        resourceIdentifier = pResourceIdentifier_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you retry a request that completed
-- successfully using the same client token and parameters, the retry
-- succeeds without performing any actions. If the parameters aren\'t
-- identical, the retry fails.
createAccessLogSubscription_clientToken :: Lens.Lens' CreateAccessLogSubscription (Prelude.Maybe Prelude.Text)
createAccessLogSubscription_clientToken = Lens.lens (\CreateAccessLogSubscription' {clientToken} -> clientToken) (\s@CreateAccessLogSubscription' {} a -> s {clientToken = a} :: CreateAccessLogSubscription)

-- | The tags for the access log subscription.
createAccessLogSubscription_tags :: Lens.Lens' CreateAccessLogSubscription (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAccessLogSubscription_tags = Lens.lens (\CreateAccessLogSubscription' {tags} -> tags) (\s@CreateAccessLogSubscription' {} a -> s {tags = a} :: CreateAccessLogSubscription) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the destination. The supported
-- destination types are CloudWatch Log groups, Kinesis Data Firehose
-- delivery streams, and Amazon S3 buckets.
createAccessLogSubscription_destinationArn :: Lens.Lens' CreateAccessLogSubscription Prelude.Text
createAccessLogSubscription_destinationArn = Lens.lens (\CreateAccessLogSubscription' {destinationArn} -> destinationArn) (\s@CreateAccessLogSubscription' {} a -> s {destinationArn = a} :: CreateAccessLogSubscription)

-- | The ID or Amazon Resource Name (ARN) of the service network or service.
createAccessLogSubscription_resourceIdentifier :: Lens.Lens' CreateAccessLogSubscription Prelude.Text
createAccessLogSubscription_resourceIdentifier = Lens.lens (\CreateAccessLogSubscription' {resourceIdentifier} -> resourceIdentifier) (\s@CreateAccessLogSubscription' {} a -> s {resourceIdentifier = a} :: CreateAccessLogSubscription)

instance Core.AWSRequest CreateAccessLogSubscription where
  type
    AWSResponse CreateAccessLogSubscription =
      CreateAccessLogSubscriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAccessLogSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "destinationArn")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "resourceArn")
            Prelude.<*> (x Data..:> "resourceId")
      )

instance Prelude.Hashable CreateAccessLogSubscription where
  hashWithSalt _salt CreateAccessLogSubscription' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` destinationArn
      `Prelude.hashWithSalt` resourceIdentifier

instance Prelude.NFData CreateAccessLogSubscription where
  rnf CreateAccessLogSubscription' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf resourceIdentifier

instance Data.ToHeaders CreateAccessLogSubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAccessLogSubscription where
  toJSON CreateAccessLogSubscription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("destinationArn" Data..= destinationArn),
            Prelude.Just
              ("resourceIdentifier" Data..= resourceIdentifier)
          ]
      )

instance Data.ToPath CreateAccessLogSubscription where
  toPath = Prelude.const "/accesslogsubscriptions"

instance Data.ToQuery CreateAccessLogSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAccessLogSubscriptionResponse' smart constructor.
data CreateAccessLogSubscriptionResponse = CreateAccessLogSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the access log subscription.
    arn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the log destination.
    destinationArn :: Prelude.Text,
    -- | The ID of the access log subscription.
    id :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the service network or service.
    resourceArn :: Prelude.Text,
    -- | The ID of the service network or service.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccessLogSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAccessLogSubscriptionResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createAccessLogSubscriptionResponse_arn' - The Amazon Resource Name (ARN) of the access log subscription.
--
-- 'destinationArn', 'createAccessLogSubscriptionResponse_destinationArn' - The Amazon Resource Name (ARN) of the log destination.
--
-- 'id', 'createAccessLogSubscriptionResponse_id' - The ID of the access log subscription.
--
-- 'resourceArn', 'createAccessLogSubscriptionResponse_resourceArn' - The Amazon Resource Name (ARN) of the service network or service.
--
-- 'resourceId', 'createAccessLogSubscriptionResponse_resourceId' - The ID of the service network or service.
newCreateAccessLogSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'destinationArn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  CreateAccessLogSubscriptionResponse
newCreateAccessLogSubscriptionResponse
  pHttpStatus_
  pArn_
  pDestinationArn_
  pId_
  pResourceArn_
  pResourceId_ =
    CreateAccessLogSubscriptionResponse'
      { httpStatus =
          pHttpStatus_,
        arn = pArn_,
        destinationArn = pDestinationArn_,
        id = pId_,
        resourceArn = pResourceArn_,
        resourceId = pResourceId_
      }

-- | The response's http status code.
createAccessLogSubscriptionResponse_httpStatus :: Lens.Lens' CreateAccessLogSubscriptionResponse Prelude.Int
createAccessLogSubscriptionResponse_httpStatus = Lens.lens (\CreateAccessLogSubscriptionResponse' {httpStatus} -> httpStatus) (\s@CreateAccessLogSubscriptionResponse' {} a -> s {httpStatus = a} :: CreateAccessLogSubscriptionResponse)

-- | The Amazon Resource Name (ARN) of the access log subscription.
createAccessLogSubscriptionResponse_arn :: Lens.Lens' CreateAccessLogSubscriptionResponse Prelude.Text
createAccessLogSubscriptionResponse_arn = Lens.lens (\CreateAccessLogSubscriptionResponse' {arn} -> arn) (\s@CreateAccessLogSubscriptionResponse' {} a -> s {arn = a} :: CreateAccessLogSubscriptionResponse)

-- | The Amazon Resource Name (ARN) of the log destination.
createAccessLogSubscriptionResponse_destinationArn :: Lens.Lens' CreateAccessLogSubscriptionResponse Prelude.Text
createAccessLogSubscriptionResponse_destinationArn = Lens.lens (\CreateAccessLogSubscriptionResponse' {destinationArn} -> destinationArn) (\s@CreateAccessLogSubscriptionResponse' {} a -> s {destinationArn = a} :: CreateAccessLogSubscriptionResponse)

-- | The ID of the access log subscription.
createAccessLogSubscriptionResponse_id :: Lens.Lens' CreateAccessLogSubscriptionResponse Prelude.Text
createAccessLogSubscriptionResponse_id = Lens.lens (\CreateAccessLogSubscriptionResponse' {id} -> id) (\s@CreateAccessLogSubscriptionResponse' {} a -> s {id = a} :: CreateAccessLogSubscriptionResponse)

-- | The Amazon Resource Name (ARN) of the service network or service.
createAccessLogSubscriptionResponse_resourceArn :: Lens.Lens' CreateAccessLogSubscriptionResponse Prelude.Text
createAccessLogSubscriptionResponse_resourceArn = Lens.lens (\CreateAccessLogSubscriptionResponse' {resourceArn} -> resourceArn) (\s@CreateAccessLogSubscriptionResponse' {} a -> s {resourceArn = a} :: CreateAccessLogSubscriptionResponse)

-- | The ID of the service network or service.
createAccessLogSubscriptionResponse_resourceId :: Lens.Lens' CreateAccessLogSubscriptionResponse Prelude.Text
createAccessLogSubscriptionResponse_resourceId = Lens.lens (\CreateAccessLogSubscriptionResponse' {resourceId} -> resourceId) (\s@CreateAccessLogSubscriptionResponse' {} a -> s {resourceId = a} :: CreateAccessLogSubscriptionResponse)

instance
  Prelude.NFData
    CreateAccessLogSubscriptionResponse
  where
  rnf CreateAccessLogSubscriptionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceId
