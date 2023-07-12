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
-- Module      : Amazonka.Pinpoint.DeleteApnsVoipSandboxChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the APNs VoIP sandbox channel for an application and deletes
-- any existing settings for the channel.
module Amazonka.Pinpoint.DeleteApnsVoipSandboxChannel
  ( -- * Creating a Request
    DeleteApnsVoipSandboxChannel (..),
    newDeleteApnsVoipSandboxChannel,

    -- * Request Lenses
    deleteApnsVoipSandboxChannel_applicationId,

    -- * Destructuring the Response
    DeleteApnsVoipSandboxChannelResponse (..),
    newDeleteApnsVoipSandboxChannelResponse,

    -- * Response Lenses
    deleteApnsVoipSandboxChannelResponse_httpStatus,
    deleteApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApnsVoipSandboxChannel' smart constructor.
data DeleteApnsVoipSandboxChannel = DeleteApnsVoipSandboxChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApnsVoipSandboxChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteApnsVoipSandboxChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteApnsVoipSandboxChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  DeleteApnsVoipSandboxChannel
newDeleteApnsVoipSandboxChannel pApplicationId_ =
  DeleteApnsVoipSandboxChannel'
    { applicationId =
        pApplicationId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteApnsVoipSandboxChannel_applicationId :: Lens.Lens' DeleteApnsVoipSandboxChannel Prelude.Text
deleteApnsVoipSandboxChannel_applicationId = Lens.lens (\DeleteApnsVoipSandboxChannel' {applicationId} -> applicationId) (\s@DeleteApnsVoipSandboxChannel' {} a -> s {applicationId = a} :: DeleteApnsVoipSandboxChannel)

instance Core.AWSRequest DeleteApnsVoipSandboxChannel where
  type
    AWSResponse DeleteApnsVoipSandboxChannel =
      DeleteApnsVoipSandboxChannelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteApnsVoipSandboxChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance
  Prelude.Hashable
    DeleteApnsVoipSandboxChannel
  where
  hashWithSalt _salt DeleteApnsVoipSandboxChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData DeleteApnsVoipSandboxChannel where
  rnf DeleteApnsVoipSandboxChannel' {..} =
    Prelude.rnf applicationId

instance Data.ToHeaders DeleteApnsVoipSandboxChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteApnsVoipSandboxChannel where
  toPath DeleteApnsVoipSandboxChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/channels/apns_voip_sandbox"
      ]

instance Data.ToQuery DeleteApnsVoipSandboxChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApnsVoipSandboxChannelResponse' smart constructor.
data DeleteApnsVoipSandboxChannelResponse = DeleteApnsVoipSandboxChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    aPNSVoipSandboxChannelResponse :: APNSVoipSandboxChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApnsVoipSandboxChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApnsVoipSandboxChannelResponse_httpStatus' - The response's http status code.
--
-- 'aPNSVoipSandboxChannelResponse', 'deleteApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse' - Undocumented member.
newDeleteApnsVoipSandboxChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'aPNSVoipSandboxChannelResponse'
  APNSVoipSandboxChannelResponse ->
  DeleteApnsVoipSandboxChannelResponse
newDeleteApnsVoipSandboxChannelResponse
  pHttpStatus_
  pAPNSVoipSandboxChannelResponse_ =
    DeleteApnsVoipSandboxChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aPNSVoipSandboxChannelResponse =
          pAPNSVoipSandboxChannelResponse_
      }

-- | The response's http status code.
deleteApnsVoipSandboxChannelResponse_httpStatus :: Lens.Lens' DeleteApnsVoipSandboxChannelResponse Prelude.Int
deleteApnsVoipSandboxChannelResponse_httpStatus = Lens.lens (\DeleteApnsVoipSandboxChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteApnsVoipSandboxChannelResponse' {} a -> s {httpStatus = a} :: DeleteApnsVoipSandboxChannelResponse)

-- | Undocumented member.
deleteApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse :: Lens.Lens' DeleteApnsVoipSandboxChannelResponse APNSVoipSandboxChannelResponse
deleteApnsVoipSandboxChannelResponse_aPNSVoipSandboxChannelResponse = Lens.lens (\DeleteApnsVoipSandboxChannelResponse' {aPNSVoipSandboxChannelResponse} -> aPNSVoipSandboxChannelResponse) (\s@DeleteApnsVoipSandboxChannelResponse' {} a -> s {aPNSVoipSandboxChannelResponse = a} :: DeleteApnsVoipSandboxChannelResponse)

instance
  Prelude.NFData
    DeleteApnsVoipSandboxChannelResponse
  where
  rnf DeleteApnsVoipSandboxChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf aPNSVoipSandboxChannelResponse
