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
-- Module      : Amazonka.Pinpoint.DeleteApnsChannel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the APNs channel for an application and deletes any existing
-- settings for the channel.
module Amazonka.Pinpoint.DeleteApnsChannel
  ( -- * Creating a Request
    DeleteApnsChannel (..),
    newDeleteApnsChannel,

    -- * Request Lenses
    deleteApnsChannel_applicationId,

    -- * Destructuring the Response
    DeleteApnsChannelResponse (..),
    newDeleteApnsChannelResponse,

    -- * Response Lenses
    deleteApnsChannelResponse_httpStatus,
    deleteApnsChannelResponse_aPNSChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApnsChannel' smart constructor.
data DeleteApnsChannel = DeleteApnsChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApnsChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteApnsChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteApnsChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  DeleteApnsChannel
newDeleteApnsChannel pApplicationId_ =
  DeleteApnsChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteApnsChannel_applicationId :: Lens.Lens' DeleteApnsChannel Prelude.Text
deleteApnsChannel_applicationId = Lens.lens (\DeleteApnsChannel' {applicationId} -> applicationId) (\s@DeleteApnsChannel' {} a -> s {applicationId = a} :: DeleteApnsChannel)

instance Core.AWSRequest DeleteApnsChannel where
  type
    AWSResponse DeleteApnsChannel =
      DeleteApnsChannelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteApnsChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteApnsChannel where
  hashWithSalt _salt DeleteApnsChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData DeleteApnsChannel where
  rnf DeleteApnsChannel' {..} =
    Prelude.rnf applicationId

instance Core.ToHeaders DeleteApnsChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteApnsChannel where
  toPath DeleteApnsChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/apns"
      ]

instance Core.ToQuery DeleteApnsChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApnsChannelResponse' smart constructor.
data DeleteApnsChannelResponse = DeleteApnsChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    aPNSChannelResponse :: APNSChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApnsChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApnsChannelResponse_httpStatus' - The response's http status code.
--
-- 'aPNSChannelResponse', 'deleteApnsChannelResponse_aPNSChannelResponse' - Undocumented member.
newDeleteApnsChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'aPNSChannelResponse'
  APNSChannelResponse ->
  DeleteApnsChannelResponse
newDeleteApnsChannelResponse
  pHttpStatus_
  pAPNSChannelResponse_ =
    DeleteApnsChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aPNSChannelResponse = pAPNSChannelResponse_
      }

-- | The response's http status code.
deleteApnsChannelResponse_httpStatus :: Lens.Lens' DeleteApnsChannelResponse Prelude.Int
deleteApnsChannelResponse_httpStatus = Lens.lens (\DeleteApnsChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteApnsChannelResponse' {} a -> s {httpStatus = a} :: DeleteApnsChannelResponse)

-- | Undocumented member.
deleteApnsChannelResponse_aPNSChannelResponse :: Lens.Lens' DeleteApnsChannelResponse APNSChannelResponse
deleteApnsChannelResponse_aPNSChannelResponse = Lens.lens (\DeleteApnsChannelResponse' {aPNSChannelResponse} -> aPNSChannelResponse) (\s@DeleteApnsChannelResponse' {} a -> s {aPNSChannelResponse = a} :: DeleteApnsChannelResponse)

instance Prelude.NFData DeleteApnsChannelResponse where
  rnf DeleteApnsChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf aPNSChannelResponse
