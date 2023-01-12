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
-- Module      : Amazonka.DevOpsGuru.RemoveNotificationChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a notification channel from DevOps Guru. A notification channel
-- is used to notify you when DevOps Guru generates an insight that
-- contains information about how to improve your operations.
module Amazonka.DevOpsGuru.RemoveNotificationChannel
  ( -- * Creating a Request
    RemoveNotificationChannel (..),
    newRemoveNotificationChannel,

    -- * Request Lenses
    removeNotificationChannel_id,

    -- * Destructuring the Response
    RemoveNotificationChannelResponse (..),
    newRemoveNotificationChannelResponse,

    -- * Response Lenses
    removeNotificationChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveNotificationChannel' smart constructor.
data RemoveNotificationChannel = RemoveNotificationChannel'
  { -- | The ID of the notification channel to be removed.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveNotificationChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'removeNotificationChannel_id' - The ID of the notification channel to be removed.
newRemoveNotificationChannel ::
  -- | 'id'
  Prelude.Text ->
  RemoveNotificationChannel
newRemoveNotificationChannel pId_ =
  RemoveNotificationChannel' {id = pId_}

-- | The ID of the notification channel to be removed.
removeNotificationChannel_id :: Lens.Lens' RemoveNotificationChannel Prelude.Text
removeNotificationChannel_id = Lens.lens (\RemoveNotificationChannel' {id} -> id) (\s@RemoveNotificationChannel' {} a -> s {id = a} :: RemoveNotificationChannel)

instance Core.AWSRequest RemoveNotificationChannel where
  type
    AWSResponse RemoveNotificationChannel =
      RemoveNotificationChannelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveNotificationChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveNotificationChannel where
  hashWithSalt _salt RemoveNotificationChannel' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData RemoveNotificationChannel where
  rnf RemoveNotificationChannel' {..} = Prelude.rnf id

instance Data.ToHeaders RemoveNotificationChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath RemoveNotificationChannel where
  toPath RemoveNotificationChannel' {..} =
    Prelude.mconcat ["/channels/", Data.toBS id]

instance Data.ToQuery RemoveNotificationChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveNotificationChannelResponse' smart constructor.
data RemoveNotificationChannelResponse = RemoveNotificationChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveNotificationChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeNotificationChannelResponse_httpStatus' - The response's http status code.
newRemoveNotificationChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveNotificationChannelResponse
newRemoveNotificationChannelResponse pHttpStatus_ =
  RemoveNotificationChannelResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
removeNotificationChannelResponse_httpStatus :: Lens.Lens' RemoveNotificationChannelResponse Prelude.Int
removeNotificationChannelResponse_httpStatus = Lens.lens (\RemoveNotificationChannelResponse' {httpStatus} -> httpStatus) (\s@RemoveNotificationChannelResponse' {} a -> s {httpStatus = a} :: RemoveNotificationChannelResponse)

instance
  Prelude.NFData
    RemoveNotificationChannelResponse
  where
  rnf RemoveNotificationChannelResponse' {..} =
    Prelude.rnf httpStatus
