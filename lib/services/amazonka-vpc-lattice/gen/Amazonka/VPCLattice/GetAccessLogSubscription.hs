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
-- Module      : Amazonka.VPCLattice.GetAccessLogSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified access log subscription.
module Amazonka.VPCLattice.GetAccessLogSubscription
  ( -- * Creating a Request
    GetAccessLogSubscription (..),
    newGetAccessLogSubscription,

    -- * Request Lenses
    getAccessLogSubscription_accessLogSubscriptionIdentifier,

    -- * Destructuring the Response
    GetAccessLogSubscriptionResponse (..),
    newGetAccessLogSubscriptionResponse,

    -- * Response Lenses
    getAccessLogSubscriptionResponse_httpStatus,
    getAccessLogSubscriptionResponse_arn,
    getAccessLogSubscriptionResponse_createdAt,
    getAccessLogSubscriptionResponse_destinationArn,
    getAccessLogSubscriptionResponse_id,
    getAccessLogSubscriptionResponse_lastUpdatedAt,
    getAccessLogSubscriptionResponse_resourceArn,
    getAccessLogSubscriptionResponse_resourceId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newGetAccessLogSubscription' smart constructor.
data GetAccessLogSubscription = GetAccessLogSubscription'
  { -- | The ID or Amazon Resource Name (ARN) of the access log subscription.
    accessLogSubscriptionIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccessLogSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessLogSubscriptionIdentifier', 'getAccessLogSubscription_accessLogSubscriptionIdentifier' - The ID or Amazon Resource Name (ARN) of the access log subscription.
newGetAccessLogSubscription ::
  -- | 'accessLogSubscriptionIdentifier'
  Prelude.Text ->
  GetAccessLogSubscription
newGetAccessLogSubscription
  pAccessLogSubscriptionIdentifier_ =
    GetAccessLogSubscription'
      { accessLogSubscriptionIdentifier =
          pAccessLogSubscriptionIdentifier_
      }

-- | The ID or Amazon Resource Name (ARN) of the access log subscription.
getAccessLogSubscription_accessLogSubscriptionIdentifier :: Lens.Lens' GetAccessLogSubscription Prelude.Text
getAccessLogSubscription_accessLogSubscriptionIdentifier = Lens.lens (\GetAccessLogSubscription' {accessLogSubscriptionIdentifier} -> accessLogSubscriptionIdentifier) (\s@GetAccessLogSubscription' {} a -> s {accessLogSubscriptionIdentifier = a} :: GetAccessLogSubscription)

instance Core.AWSRequest GetAccessLogSubscription where
  type
    AWSResponse GetAccessLogSubscription =
      GetAccessLogSubscriptionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccessLogSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "createdAt")
            Prelude.<*> (x Data..:> "destinationArn")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "lastUpdatedAt")
            Prelude.<*> (x Data..:> "resourceArn")
            Prelude.<*> (x Data..:> "resourceId")
      )

instance Prelude.Hashable GetAccessLogSubscription where
  hashWithSalt _salt GetAccessLogSubscription' {..} =
    _salt
      `Prelude.hashWithSalt` accessLogSubscriptionIdentifier

instance Prelude.NFData GetAccessLogSubscription where
  rnf GetAccessLogSubscription' {..} =
    Prelude.rnf accessLogSubscriptionIdentifier

instance Data.ToHeaders GetAccessLogSubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAccessLogSubscription where
  toPath GetAccessLogSubscription' {..} =
    Prelude.mconcat
      [ "/accesslogsubscriptions/",
        Data.toBS accessLogSubscriptionIdentifier
      ]

instance Data.ToQuery GetAccessLogSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAccessLogSubscriptionResponse' smart constructor.
data GetAccessLogSubscriptionResponse = GetAccessLogSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the access log subscription.
    arn :: Prelude.Text,
    -- | The date and time that the access log subscription was created,
    -- specified in ISO-8601 format.
    createdAt :: Data.ISO8601,
    -- | The Amazon Resource Name (ARN) of the access log destination.
    destinationArn :: Prelude.Text,
    -- | The ID of the access log subscription.
    id :: Prelude.Text,
    -- | The date and time that the access log subscription was last updated,
    -- specified in ISO-8601 format.
    lastUpdatedAt :: Data.ISO8601,
    -- | The Amazon Resource Name (ARN) of the service network or service.
    resourceArn :: Prelude.Text,
    -- | The ID of the service network or service.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccessLogSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getAccessLogSubscriptionResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getAccessLogSubscriptionResponse_arn' - The Amazon Resource Name (ARN) of the access log subscription.
--
-- 'createdAt', 'getAccessLogSubscriptionResponse_createdAt' - The date and time that the access log subscription was created,
-- specified in ISO-8601 format.
--
-- 'destinationArn', 'getAccessLogSubscriptionResponse_destinationArn' - The Amazon Resource Name (ARN) of the access log destination.
--
-- 'id', 'getAccessLogSubscriptionResponse_id' - The ID of the access log subscription.
--
-- 'lastUpdatedAt', 'getAccessLogSubscriptionResponse_lastUpdatedAt' - The date and time that the access log subscription was last updated,
-- specified in ISO-8601 format.
--
-- 'resourceArn', 'getAccessLogSubscriptionResponse_resourceArn' - The Amazon Resource Name (ARN) of the service network or service.
--
-- 'resourceId', 'getAccessLogSubscriptionResponse_resourceId' - The ID of the service network or service.
newGetAccessLogSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'destinationArn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'lastUpdatedAt'
  Prelude.UTCTime ->
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  GetAccessLogSubscriptionResponse
newGetAccessLogSubscriptionResponse
  pHttpStatus_
  pArn_
  pCreatedAt_
  pDestinationArn_
  pId_
  pLastUpdatedAt_
  pResourceArn_
  pResourceId_ =
    GetAccessLogSubscriptionResponse'
      { httpStatus =
          pHttpStatus_,
        arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        destinationArn = pDestinationArn_,
        id = pId_,
        lastUpdatedAt =
          Data._Time Lens.# pLastUpdatedAt_,
        resourceArn = pResourceArn_,
        resourceId = pResourceId_
      }

-- | The response's http status code.
getAccessLogSubscriptionResponse_httpStatus :: Lens.Lens' GetAccessLogSubscriptionResponse Prelude.Int
getAccessLogSubscriptionResponse_httpStatus = Lens.lens (\GetAccessLogSubscriptionResponse' {httpStatus} -> httpStatus) (\s@GetAccessLogSubscriptionResponse' {} a -> s {httpStatus = a} :: GetAccessLogSubscriptionResponse)

-- | The Amazon Resource Name (ARN) of the access log subscription.
getAccessLogSubscriptionResponse_arn :: Lens.Lens' GetAccessLogSubscriptionResponse Prelude.Text
getAccessLogSubscriptionResponse_arn = Lens.lens (\GetAccessLogSubscriptionResponse' {arn} -> arn) (\s@GetAccessLogSubscriptionResponse' {} a -> s {arn = a} :: GetAccessLogSubscriptionResponse)

-- | The date and time that the access log subscription was created,
-- specified in ISO-8601 format.
getAccessLogSubscriptionResponse_createdAt :: Lens.Lens' GetAccessLogSubscriptionResponse Prelude.UTCTime
getAccessLogSubscriptionResponse_createdAt = Lens.lens (\GetAccessLogSubscriptionResponse' {createdAt} -> createdAt) (\s@GetAccessLogSubscriptionResponse' {} a -> s {createdAt = a} :: GetAccessLogSubscriptionResponse) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) of the access log destination.
getAccessLogSubscriptionResponse_destinationArn :: Lens.Lens' GetAccessLogSubscriptionResponse Prelude.Text
getAccessLogSubscriptionResponse_destinationArn = Lens.lens (\GetAccessLogSubscriptionResponse' {destinationArn} -> destinationArn) (\s@GetAccessLogSubscriptionResponse' {} a -> s {destinationArn = a} :: GetAccessLogSubscriptionResponse)

-- | The ID of the access log subscription.
getAccessLogSubscriptionResponse_id :: Lens.Lens' GetAccessLogSubscriptionResponse Prelude.Text
getAccessLogSubscriptionResponse_id = Lens.lens (\GetAccessLogSubscriptionResponse' {id} -> id) (\s@GetAccessLogSubscriptionResponse' {} a -> s {id = a} :: GetAccessLogSubscriptionResponse)

-- | The date and time that the access log subscription was last updated,
-- specified in ISO-8601 format.
getAccessLogSubscriptionResponse_lastUpdatedAt :: Lens.Lens' GetAccessLogSubscriptionResponse Prelude.UTCTime
getAccessLogSubscriptionResponse_lastUpdatedAt = Lens.lens (\GetAccessLogSubscriptionResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetAccessLogSubscriptionResponse' {} a -> s {lastUpdatedAt = a} :: GetAccessLogSubscriptionResponse) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) of the service network or service.
getAccessLogSubscriptionResponse_resourceArn :: Lens.Lens' GetAccessLogSubscriptionResponse Prelude.Text
getAccessLogSubscriptionResponse_resourceArn = Lens.lens (\GetAccessLogSubscriptionResponse' {resourceArn} -> resourceArn) (\s@GetAccessLogSubscriptionResponse' {} a -> s {resourceArn = a} :: GetAccessLogSubscriptionResponse)

-- | The ID of the service network or service.
getAccessLogSubscriptionResponse_resourceId :: Lens.Lens' GetAccessLogSubscriptionResponse Prelude.Text
getAccessLogSubscriptionResponse_resourceId = Lens.lens (\GetAccessLogSubscriptionResponse' {resourceId} -> resourceId) (\s@GetAccessLogSubscriptionResponse' {} a -> s {resourceId = a} :: GetAccessLogSubscriptionResponse)

instance
  Prelude.NFData
    GetAccessLogSubscriptionResponse
  where
  rnf GetAccessLogSubscriptionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceId
