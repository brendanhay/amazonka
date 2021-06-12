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
-- Module      : Network.AWS.WorkDocs.DeleteNotificationSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified subscription from the specified organization.
module Network.AWS.WorkDocs.DeleteNotificationSubscription
  ( -- * Creating a Request
    DeleteNotificationSubscription (..),
    newDeleteNotificationSubscription,

    -- * Request Lenses
    deleteNotificationSubscription_subscriptionId,
    deleteNotificationSubscription_organizationId,

    -- * Destructuring the Response
    DeleteNotificationSubscriptionResponse (..),
    newDeleteNotificationSubscriptionResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDeleteNotificationSubscription' smart constructor.
data DeleteNotificationSubscription = DeleteNotificationSubscription'
  { -- | The ID of the subscription.
    subscriptionId :: Core.Text,
    -- | The ID of the organization.
    organizationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNotificationSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptionId', 'deleteNotificationSubscription_subscriptionId' - The ID of the subscription.
--
-- 'organizationId', 'deleteNotificationSubscription_organizationId' - The ID of the organization.
newDeleteNotificationSubscription ::
  -- | 'subscriptionId'
  Core.Text ->
  -- | 'organizationId'
  Core.Text ->
  DeleteNotificationSubscription
newDeleteNotificationSubscription
  pSubscriptionId_
  pOrganizationId_ =
    DeleteNotificationSubscription'
      { subscriptionId =
          pSubscriptionId_,
        organizationId = pOrganizationId_
      }

-- | The ID of the subscription.
deleteNotificationSubscription_subscriptionId :: Lens.Lens' DeleteNotificationSubscription Core.Text
deleteNotificationSubscription_subscriptionId = Lens.lens (\DeleteNotificationSubscription' {subscriptionId} -> subscriptionId) (\s@DeleteNotificationSubscription' {} a -> s {subscriptionId = a} :: DeleteNotificationSubscription)

-- | The ID of the organization.
deleteNotificationSubscription_organizationId :: Lens.Lens' DeleteNotificationSubscription Core.Text
deleteNotificationSubscription_organizationId = Lens.lens (\DeleteNotificationSubscription' {organizationId} -> organizationId) (\s@DeleteNotificationSubscription' {} a -> s {organizationId = a} :: DeleteNotificationSubscription)

instance
  Core.AWSRequest
    DeleteNotificationSubscription
  where
  type
    AWSResponse DeleteNotificationSubscription =
      DeleteNotificationSubscriptionResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteNotificationSubscriptionResponse'

instance Core.Hashable DeleteNotificationSubscription

instance Core.NFData DeleteNotificationSubscription

instance
  Core.ToHeaders
    DeleteNotificationSubscription
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteNotificationSubscription where
  toPath DeleteNotificationSubscription' {..} =
    Core.mconcat
      [ "/api/v1/organizations/",
        Core.toBS organizationId,
        "/subscriptions/",
        Core.toBS subscriptionId
      ]

instance Core.ToQuery DeleteNotificationSubscription where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteNotificationSubscriptionResponse' smart constructor.
data DeleteNotificationSubscriptionResponse = DeleteNotificationSubscriptionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNotificationSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteNotificationSubscriptionResponse ::
  DeleteNotificationSubscriptionResponse
newDeleteNotificationSubscriptionResponse =
  DeleteNotificationSubscriptionResponse'

instance
  Core.NFData
    DeleteNotificationSubscriptionResponse
