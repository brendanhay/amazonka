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
-- Module      : Network.AWS.RDS.DeleteEventSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an RDS event notification subscription.
module Network.AWS.RDS.DeleteEventSubscription
  ( -- * Creating a Request
    DeleteEventSubscription (..),
    newDeleteEventSubscription,

    -- * Request Lenses
    deleteEventSubscription_subscriptionName,

    -- * Destructuring the Response
    DeleteEventSubscriptionResponse (..),
    newDeleteEventSubscriptionResponse,

    -- * Response Lenses
    deleteEventSubscriptionResponse_eventSubscription,
    deleteEventSubscriptionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteEventSubscription' smart constructor.
data DeleteEventSubscription = DeleteEventSubscription'
  { -- | The name of the RDS event notification subscription you want to delete.
    subscriptionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteEventSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptionName', 'deleteEventSubscription_subscriptionName' - The name of the RDS event notification subscription you want to delete.
newDeleteEventSubscription ::
  -- | 'subscriptionName'
  Core.Text ->
  DeleteEventSubscription
newDeleteEventSubscription pSubscriptionName_ =
  DeleteEventSubscription'
    { subscriptionName =
        pSubscriptionName_
    }

-- | The name of the RDS event notification subscription you want to delete.
deleteEventSubscription_subscriptionName :: Lens.Lens' DeleteEventSubscription Core.Text
deleteEventSubscription_subscriptionName = Lens.lens (\DeleteEventSubscription' {subscriptionName} -> subscriptionName) (\s@DeleteEventSubscription' {} a -> s {subscriptionName = a} :: DeleteEventSubscription)

instance Core.AWSRequest DeleteEventSubscription where
  type
    AWSResponse DeleteEventSubscription =
      DeleteEventSubscriptionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteEventSubscriptionResult"
      ( \s h x ->
          DeleteEventSubscriptionResponse'
            Core.<$> (x Core..@? "EventSubscription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteEventSubscription

instance Core.NFData DeleteEventSubscription

instance Core.ToHeaders DeleteEventSubscription where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteEventSubscription where
  toPath = Core.const "/"

instance Core.ToQuery DeleteEventSubscription where
  toQuery DeleteEventSubscription' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteEventSubscription" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "SubscriptionName" Core.=: subscriptionName
      ]

-- | /See:/ 'newDeleteEventSubscriptionResponse' smart constructor.
data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse'
  { eventSubscription :: Core.Maybe EventSubscription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteEventSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSubscription', 'deleteEventSubscriptionResponse_eventSubscription' - Undocumented member.
--
-- 'httpStatus', 'deleteEventSubscriptionResponse_httpStatus' - The response's http status code.
newDeleteEventSubscriptionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteEventSubscriptionResponse
newDeleteEventSubscriptionResponse pHttpStatus_ =
  DeleteEventSubscriptionResponse'
    { eventSubscription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteEventSubscriptionResponse_eventSubscription :: Lens.Lens' DeleteEventSubscriptionResponse (Core.Maybe EventSubscription)
deleteEventSubscriptionResponse_eventSubscription = Lens.lens (\DeleteEventSubscriptionResponse' {eventSubscription} -> eventSubscription) (\s@DeleteEventSubscriptionResponse' {} a -> s {eventSubscription = a} :: DeleteEventSubscriptionResponse)

-- | The response's http status code.
deleteEventSubscriptionResponse_httpStatus :: Lens.Lens' DeleteEventSubscriptionResponse Core.Int
deleteEventSubscriptionResponse_httpStatus = Lens.lens (\DeleteEventSubscriptionResponse' {httpStatus} -> httpStatus) (\s@DeleteEventSubscriptionResponse' {} a -> s {httpStatus = a} :: DeleteEventSubscriptionResponse)

instance Core.NFData DeleteEventSubscriptionResponse
