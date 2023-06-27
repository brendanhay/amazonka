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
-- Module      : Amazonka.VPCLattice.UpdateAccessLogSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified access log subscription.
module Amazonka.VPCLattice.UpdateAccessLogSubscription
  ( -- * Creating a Request
    UpdateAccessLogSubscription (..),
    newUpdateAccessLogSubscription,

    -- * Request Lenses
    updateAccessLogSubscription_accessLogSubscriptionIdentifier,
    updateAccessLogSubscription_destinationArn,

    -- * Destructuring the Response
    UpdateAccessLogSubscriptionResponse (..),
    newUpdateAccessLogSubscriptionResponse,

    -- * Response Lenses
    updateAccessLogSubscriptionResponse_httpStatus,
    updateAccessLogSubscriptionResponse_arn,
    updateAccessLogSubscriptionResponse_destinationArn,
    updateAccessLogSubscriptionResponse_id,
    updateAccessLogSubscriptionResponse_resourceArn,
    updateAccessLogSubscriptionResponse_resourceId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newUpdateAccessLogSubscription' smart constructor.
data UpdateAccessLogSubscription = UpdateAccessLogSubscription'
  { -- | The ID or Amazon Resource Name (ARN) of the access log subscription.
    accessLogSubscriptionIdentifier :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the access log destination.
    destinationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccessLogSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessLogSubscriptionIdentifier', 'updateAccessLogSubscription_accessLogSubscriptionIdentifier' - The ID or Amazon Resource Name (ARN) of the access log subscription.
--
-- 'destinationArn', 'updateAccessLogSubscription_destinationArn' - The Amazon Resource Name (ARN) of the access log destination.
newUpdateAccessLogSubscription ::
  -- | 'accessLogSubscriptionIdentifier'
  Prelude.Text ->
  -- | 'destinationArn'
  Prelude.Text ->
  UpdateAccessLogSubscription
newUpdateAccessLogSubscription
  pAccessLogSubscriptionIdentifier_
  pDestinationArn_ =
    UpdateAccessLogSubscription'
      { accessLogSubscriptionIdentifier =
          pAccessLogSubscriptionIdentifier_,
        destinationArn = pDestinationArn_
      }

-- | The ID or Amazon Resource Name (ARN) of the access log subscription.
updateAccessLogSubscription_accessLogSubscriptionIdentifier :: Lens.Lens' UpdateAccessLogSubscription Prelude.Text
updateAccessLogSubscription_accessLogSubscriptionIdentifier = Lens.lens (\UpdateAccessLogSubscription' {accessLogSubscriptionIdentifier} -> accessLogSubscriptionIdentifier) (\s@UpdateAccessLogSubscription' {} a -> s {accessLogSubscriptionIdentifier = a} :: UpdateAccessLogSubscription)

-- | The Amazon Resource Name (ARN) of the access log destination.
updateAccessLogSubscription_destinationArn :: Lens.Lens' UpdateAccessLogSubscription Prelude.Text
updateAccessLogSubscription_destinationArn = Lens.lens (\UpdateAccessLogSubscription' {destinationArn} -> destinationArn) (\s@UpdateAccessLogSubscription' {} a -> s {destinationArn = a} :: UpdateAccessLogSubscription)

instance Core.AWSRequest UpdateAccessLogSubscription where
  type
    AWSResponse UpdateAccessLogSubscription =
      UpdateAccessLogSubscriptionResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAccessLogSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "destinationArn")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "resourceArn")
            Prelude.<*> (x Data..:> "resourceId")
      )

instance Prelude.Hashable UpdateAccessLogSubscription where
  hashWithSalt _salt UpdateAccessLogSubscription' {..} =
    _salt
      `Prelude.hashWithSalt` accessLogSubscriptionIdentifier
      `Prelude.hashWithSalt` destinationArn

instance Prelude.NFData UpdateAccessLogSubscription where
  rnf UpdateAccessLogSubscription' {..} =
    Prelude.rnf accessLogSubscriptionIdentifier
      `Prelude.seq` Prelude.rnf destinationArn

instance Data.ToHeaders UpdateAccessLogSubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAccessLogSubscription where
  toJSON UpdateAccessLogSubscription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("destinationArn" Data..= destinationArn)
          ]
      )

instance Data.ToPath UpdateAccessLogSubscription where
  toPath UpdateAccessLogSubscription' {..} =
    Prelude.mconcat
      [ "/accesslogsubscriptions/",
        Data.toBS accessLogSubscriptionIdentifier
      ]

instance Data.ToQuery UpdateAccessLogSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAccessLogSubscriptionResponse' smart constructor.
data UpdateAccessLogSubscriptionResponse = UpdateAccessLogSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the access log subscription.
    arn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the access log destination.
    destinationArn :: Prelude.Text,
    -- | The ID of the access log subscription.
    id :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the access log subscription.
    resourceArn :: Prelude.Text,
    -- | The ID of the resource.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccessLogSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAccessLogSubscriptionResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'updateAccessLogSubscriptionResponse_arn' - The Amazon Resource Name (ARN) of the access log subscription.
--
-- 'destinationArn', 'updateAccessLogSubscriptionResponse_destinationArn' - The Amazon Resource Name (ARN) of the access log destination.
--
-- 'id', 'updateAccessLogSubscriptionResponse_id' - The ID of the access log subscription.
--
-- 'resourceArn', 'updateAccessLogSubscriptionResponse_resourceArn' - The Amazon Resource Name (ARN) of the access log subscription.
--
-- 'resourceId', 'updateAccessLogSubscriptionResponse_resourceId' - The ID of the resource.
newUpdateAccessLogSubscriptionResponse ::
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
  UpdateAccessLogSubscriptionResponse
newUpdateAccessLogSubscriptionResponse
  pHttpStatus_
  pArn_
  pDestinationArn_
  pId_
  pResourceArn_
  pResourceId_ =
    UpdateAccessLogSubscriptionResponse'
      { httpStatus =
          pHttpStatus_,
        arn = pArn_,
        destinationArn = pDestinationArn_,
        id = pId_,
        resourceArn = pResourceArn_,
        resourceId = pResourceId_
      }

-- | The response's http status code.
updateAccessLogSubscriptionResponse_httpStatus :: Lens.Lens' UpdateAccessLogSubscriptionResponse Prelude.Int
updateAccessLogSubscriptionResponse_httpStatus = Lens.lens (\UpdateAccessLogSubscriptionResponse' {httpStatus} -> httpStatus) (\s@UpdateAccessLogSubscriptionResponse' {} a -> s {httpStatus = a} :: UpdateAccessLogSubscriptionResponse)

-- | The Amazon Resource Name (ARN) of the access log subscription.
updateAccessLogSubscriptionResponse_arn :: Lens.Lens' UpdateAccessLogSubscriptionResponse Prelude.Text
updateAccessLogSubscriptionResponse_arn = Lens.lens (\UpdateAccessLogSubscriptionResponse' {arn} -> arn) (\s@UpdateAccessLogSubscriptionResponse' {} a -> s {arn = a} :: UpdateAccessLogSubscriptionResponse)

-- | The Amazon Resource Name (ARN) of the access log destination.
updateAccessLogSubscriptionResponse_destinationArn :: Lens.Lens' UpdateAccessLogSubscriptionResponse Prelude.Text
updateAccessLogSubscriptionResponse_destinationArn = Lens.lens (\UpdateAccessLogSubscriptionResponse' {destinationArn} -> destinationArn) (\s@UpdateAccessLogSubscriptionResponse' {} a -> s {destinationArn = a} :: UpdateAccessLogSubscriptionResponse)

-- | The ID of the access log subscription.
updateAccessLogSubscriptionResponse_id :: Lens.Lens' UpdateAccessLogSubscriptionResponse Prelude.Text
updateAccessLogSubscriptionResponse_id = Lens.lens (\UpdateAccessLogSubscriptionResponse' {id} -> id) (\s@UpdateAccessLogSubscriptionResponse' {} a -> s {id = a} :: UpdateAccessLogSubscriptionResponse)

-- | The Amazon Resource Name (ARN) of the access log subscription.
updateAccessLogSubscriptionResponse_resourceArn :: Lens.Lens' UpdateAccessLogSubscriptionResponse Prelude.Text
updateAccessLogSubscriptionResponse_resourceArn = Lens.lens (\UpdateAccessLogSubscriptionResponse' {resourceArn} -> resourceArn) (\s@UpdateAccessLogSubscriptionResponse' {} a -> s {resourceArn = a} :: UpdateAccessLogSubscriptionResponse)

-- | The ID of the resource.
updateAccessLogSubscriptionResponse_resourceId :: Lens.Lens' UpdateAccessLogSubscriptionResponse Prelude.Text
updateAccessLogSubscriptionResponse_resourceId = Lens.lens (\UpdateAccessLogSubscriptionResponse' {resourceId} -> resourceId) (\s@UpdateAccessLogSubscriptionResponse' {} a -> s {resourceId = a} :: UpdateAccessLogSubscriptionResponse)

instance
  Prelude.NFData
    UpdateAccessLogSubscriptionResponse
  where
  rnf UpdateAccessLogSubscriptionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceId
