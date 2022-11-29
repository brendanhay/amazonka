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
-- Module      : Amazonka.DocumentDB.DeleteEventSubscription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon DocumentDB event notification subscription.
module Amazonka.DocumentDB.DeleteEventSubscription
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DocumentDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input to DeleteEventSubscription.
--
-- /See:/ 'newDeleteEventSubscription' smart constructor.
data DeleteEventSubscription = DeleteEventSubscription'
  { -- | The name of the Amazon DocumentDB event notification subscription that
    -- you want to delete.
    subscriptionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptionName', 'deleteEventSubscription_subscriptionName' - The name of the Amazon DocumentDB event notification subscription that
-- you want to delete.
newDeleteEventSubscription ::
  -- | 'subscriptionName'
  Prelude.Text ->
  DeleteEventSubscription
newDeleteEventSubscription pSubscriptionName_ =
  DeleteEventSubscription'
    { subscriptionName =
        pSubscriptionName_
    }

-- | The name of the Amazon DocumentDB event notification subscription that
-- you want to delete.
deleteEventSubscription_subscriptionName :: Lens.Lens' DeleteEventSubscription Prelude.Text
deleteEventSubscription_subscriptionName = Lens.lens (\DeleteEventSubscription' {subscriptionName} -> subscriptionName) (\s@DeleteEventSubscription' {} a -> s {subscriptionName = a} :: DeleteEventSubscription)

instance Core.AWSRequest DeleteEventSubscription where
  type
    AWSResponse DeleteEventSubscription =
      DeleteEventSubscriptionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteEventSubscriptionResult"
      ( \s h x ->
          DeleteEventSubscriptionResponse'
            Prelude.<$> (x Core..@? "EventSubscription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEventSubscription where
  hashWithSalt _salt DeleteEventSubscription' {..} =
    _salt `Prelude.hashWithSalt` subscriptionName

instance Prelude.NFData DeleteEventSubscription where
  rnf DeleteEventSubscription' {..} =
    Prelude.rnf subscriptionName

instance Core.ToHeaders DeleteEventSubscription where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteEventSubscription where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteEventSubscription where
  toQuery DeleteEventSubscription' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteEventSubscription" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "SubscriptionName" Core.=: subscriptionName
      ]

-- | /See:/ 'newDeleteEventSubscriptionResponse' smart constructor.
data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse'
  { eventSubscription :: Prelude.Maybe EventSubscription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteEventSubscriptionResponse
newDeleteEventSubscriptionResponse pHttpStatus_ =
  DeleteEventSubscriptionResponse'
    { eventSubscription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteEventSubscriptionResponse_eventSubscription :: Lens.Lens' DeleteEventSubscriptionResponse (Prelude.Maybe EventSubscription)
deleteEventSubscriptionResponse_eventSubscription = Lens.lens (\DeleteEventSubscriptionResponse' {eventSubscription} -> eventSubscription) (\s@DeleteEventSubscriptionResponse' {} a -> s {eventSubscription = a} :: DeleteEventSubscriptionResponse)

-- | The response's http status code.
deleteEventSubscriptionResponse_httpStatus :: Lens.Lens' DeleteEventSubscriptionResponse Prelude.Int
deleteEventSubscriptionResponse_httpStatus = Lens.lens (\DeleteEventSubscriptionResponse' {httpStatus} -> httpStatus) (\s@DeleteEventSubscriptionResponse' {} a -> s {httpStatus = a} :: DeleteEventSubscriptionResponse)

instance
  Prelude.NFData
    DeleteEventSubscriptionResponse
  where
  rnf DeleteEventSubscriptionResponse' {..} =
    Prelude.rnf eventSubscription
      `Prelude.seq` Prelude.rnf httpStatus
