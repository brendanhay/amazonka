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
-- Module      : Amazonka.SecurityLake.DeleteSubscriberNotification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified notification subscription in Amazon Security Lake
-- for the organization you specify.
module Amazonka.SecurityLake.DeleteSubscriberNotification
  ( -- * Creating a Request
    DeleteSubscriberNotification (..),
    newDeleteSubscriberNotification,

    -- * Request Lenses
    deleteSubscriberNotification_subscriberId,

    -- * Destructuring the Response
    DeleteSubscriberNotificationResponse (..),
    newDeleteSubscriberNotificationResponse,

    -- * Response Lenses
    deleteSubscriberNotificationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newDeleteSubscriberNotification' smart constructor.
data DeleteSubscriberNotification = DeleteSubscriberNotification'
  { -- | The ID of the Security Lake subscriber account.
    subscriberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSubscriberNotification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriberId', 'deleteSubscriberNotification_subscriberId' - The ID of the Security Lake subscriber account.
newDeleteSubscriberNotification ::
  -- | 'subscriberId'
  Prelude.Text ->
  DeleteSubscriberNotification
newDeleteSubscriberNotification pSubscriberId_ =
  DeleteSubscriberNotification'
    { subscriberId =
        pSubscriberId_
    }

-- | The ID of the Security Lake subscriber account.
deleteSubscriberNotification_subscriberId :: Lens.Lens' DeleteSubscriberNotification Prelude.Text
deleteSubscriberNotification_subscriberId = Lens.lens (\DeleteSubscriberNotification' {subscriberId} -> subscriberId) (\s@DeleteSubscriberNotification' {} a -> s {subscriberId = a} :: DeleteSubscriberNotification)

instance Core.AWSRequest DeleteSubscriberNotification where
  type
    AWSResponse DeleteSubscriberNotification =
      DeleteSubscriberNotificationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSubscriberNotificationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteSubscriberNotification
  where
  hashWithSalt _salt DeleteSubscriberNotification' {..} =
    _salt `Prelude.hashWithSalt` subscriberId

instance Prelude.NFData DeleteSubscriberNotification where
  rnf DeleteSubscriberNotification' {..} =
    Prelude.rnf subscriberId

instance Data.ToHeaders DeleteSubscriberNotification where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSubscriberNotification where
  toPath DeleteSubscriberNotification' {..} =
    Prelude.mconcat
      [ "/v1/subscribers/",
        Data.toBS subscriberId,
        "/notification"
      ]

instance Data.ToQuery DeleteSubscriberNotification where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSubscriberNotificationResponse' smart constructor.
data DeleteSubscriberNotificationResponse = DeleteSubscriberNotificationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSubscriberNotificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSubscriberNotificationResponse_httpStatus' - The response's http status code.
newDeleteSubscriberNotificationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSubscriberNotificationResponse
newDeleteSubscriberNotificationResponse pHttpStatus_ =
  DeleteSubscriberNotificationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSubscriberNotificationResponse_httpStatus :: Lens.Lens' DeleteSubscriberNotificationResponse Prelude.Int
deleteSubscriberNotificationResponse_httpStatus = Lens.lens (\DeleteSubscriberNotificationResponse' {httpStatus} -> httpStatus) (\s@DeleteSubscriberNotificationResponse' {} a -> s {httpStatus = a} :: DeleteSubscriberNotificationResponse)

instance
  Prelude.NFData
    DeleteSubscriberNotificationResponse
  where
  rnf DeleteSubscriberNotificationResponse' {..} =
    Prelude.rnf httpStatus
