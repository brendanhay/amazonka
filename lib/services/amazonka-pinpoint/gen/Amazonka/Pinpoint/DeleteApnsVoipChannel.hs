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
-- Module      : Amazonka.Pinpoint.DeleteApnsVoipChannel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the APNs VoIP channel for an application and deletes any
-- existing settings for the channel.
module Amazonka.Pinpoint.DeleteApnsVoipChannel
  ( -- * Creating a Request
    DeleteApnsVoipChannel (..),
    newDeleteApnsVoipChannel,

    -- * Request Lenses
    deleteApnsVoipChannel_applicationId,

    -- * Destructuring the Response
    DeleteApnsVoipChannelResponse (..),
    newDeleteApnsVoipChannelResponse,

    -- * Response Lenses
    deleteApnsVoipChannelResponse_httpStatus,
    deleteApnsVoipChannelResponse_aPNSVoipChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApnsVoipChannel' smart constructor.
data DeleteApnsVoipChannel = DeleteApnsVoipChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApnsVoipChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteApnsVoipChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteApnsVoipChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  DeleteApnsVoipChannel
newDeleteApnsVoipChannel pApplicationId_ =
  DeleteApnsVoipChannel'
    { applicationId =
        pApplicationId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteApnsVoipChannel_applicationId :: Lens.Lens' DeleteApnsVoipChannel Prelude.Text
deleteApnsVoipChannel_applicationId = Lens.lens (\DeleteApnsVoipChannel' {applicationId} -> applicationId) (\s@DeleteApnsVoipChannel' {} a -> s {applicationId = a} :: DeleteApnsVoipChannel)

instance Core.AWSRequest DeleteApnsVoipChannel where
  type
    AWSResponse DeleteApnsVoipChannel =
      DeleteApnsVoipChannelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteApnsVoipChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteApnsVoipChannel where
  hashWithSalt _salt DeleteApnsVoipChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData DeleteApnsVoipChannel where
  rnf DeleteApnsVoipChannel' {..} =
    Prelude.rnf applicationId

instance Core.ToHeaders DeleteApnsVoipChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteApnsVoipChannel where
  toPath DeleteApnsVoipChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/apns_voip"
      ]

instance Core.ToQuery DeleteApnsVoipChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApnsVoipChannelResponse' smart constructor.
data DeleteApnsVoipChannelResponse = DeleteApnsVoipChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    aPNSVoipChannelResponse :: APNSVoipChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApnsVoipChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApnsVoipChannelResponse_httpStatus' - The response's http status code.
--
-- 'aPNSVoipChannelResponse', 'deleteApnsVoipChannelResponse_aPNSVoipChannelResponse' - Undocumented member.
newDeleteApnsVoipChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'aPNSVoipChannelResponse'
  APNSVoipChannelResponse ->
  DeleteApnsVoipChannelResponse
newDeleteApnsVoipChannelResponse
  pHttpStatus_
  pAPNSVoipChannelResponse_ =
    DeleteApnsVoipChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aPNSVoipChannelResponse =
          pAPNSVoipChannelResponse_
      }

-- | The response's http status code.
deleteApnsVoipChannelResponse_httpStatus :: Lens.Lens' DeleteApnsVoipChannelResponse Prelude.Int
deleteApnsVoipChannelResponse_httpStatus = Lens.lens (\DeleteApnsVoipChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteApnsVoipChannelResponse' {} a -> s {httpStatus = a} :: DeleteApnsVoipChannelResponse)

-- | Undocumented member.
deleteApnsVoipChannelResponse_aPNSVoipChannelResponse :: Lens.Lens' DeleteApnsVoipChannelResponse APNSVoipChannelResponse
deleteApnsVoipChannelResponse_aPNSVoipChannelResponse = Lens.lens (\DeleteApnsVoipChannelResponse' {aPNSVoipChannelResponse} -> aPNSVoipChannelResponse) (\s@DeleteApnsVoipChannelResponse' {} a -> s {aPNSVoipChannelResponse = a} :: DeleteApnsVoipChannelResponse)

instance Prelude.NFData DeleteApnsVoipChannelResponse where
  rnf DeleteApnsVoipChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf aPNSVoipChannelResponse
