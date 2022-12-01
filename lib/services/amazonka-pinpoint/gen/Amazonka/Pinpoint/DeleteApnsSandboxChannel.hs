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
-- Module      : Amazonka.Pinpoint.DeleteApnsSandboxChannel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the APNs sandbox channel for an application and deletes any
-- existing settings for the channel.
module Amazonka.Pinpoint.DeleteApnsSandboxChannel
  ( -- * Creating a Request
    DeleteApnsSandboxChannel (..),
    newDeleteApnsSandboxChannel,

    -- * Request Lenses
    deleteApnsSandboxChannel_applicationId,

    -- * Destructuring the Response
    DeleteApnsSandboxChannelResponse (..),
    newDeleteApnsSandboxChannelResponse,

    -- * Response Lenses
    deleteApnsSandboxChannelResponse_httpStatus,
    deleteApnsSandboxChannelResponse_aPNSSandboxChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApnsSandboxChannel' smart constructor.
data DeleteApnsSandboxChannel = DeleteApnsSandboxChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApnsSandboxChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteApnsSandboxChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteApnsSandboxChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  DeleteApnsSandboxChannel
newDeleteApnsSandboxChannel pApplicationId_ =
  DeleteApnsSandboxChannel'
    { applicationId =
        pApplicationId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteApnsSandboxChannel_applicationId :: Lens.Lens' DeleteApnsSandboxChannel Prelude.Text
deleteApnsSandboxChannel_applicationId = Lens.lens (\DeleteApnsSandboxChannel' {applicationId} -> applicationId) (\s@DeleteApnsSandboxChannel' {} a -> s {applicationId = a} :: DeleteApnsSandboxChannel)

instance Core.AWSRequest DeleteApnsSandboxChannel where
  type
    AWSResponse DeleteApnsSandboxChannel =
      DeleteApnsSandboxChannelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteApnsSandboxChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteApnsSandboxChannel where
  hashWithSalt _salt DeleteApnsSandboxChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData DeleteApnsSandboxChannel where
  rnf DeleteApnsSandboxChannel' {..} =
    Prelude.rnf applicationId

instance Core.ToHeaders DeleteApnsSandboxChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteApnsSandboxChannel where
  toPath DeleteApnsSandboxChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/apns_sandbox"
      ]

instance Core.ToQuery DeleteApnsSandboxChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApnsSandboxChannelResponse' smart constructor.
data DeleteApnsSandboxChannelResponse = DeleteApnsSandboxChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    aPNSSandboxChannelResponse :: APNSSandboxChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApnsSandboxChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApnsSandboxChannelResponse_httpStatus' - The response's http status code.
--
-- 'aPNSSandboxChannelResponse', 'deleteApnsSandboxChannelResponse_aPNSSandboxChannelResponse' - Undocumented member.
newDeleteApnsSandboxChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'aPNSSandboxChannelResponse'
  APNSSandboxChannelResponse ->
  DeleteApnsSandboxChannelResponse
newDeleteApnsSandboxChannelResponse
  pHttpStatus_
  pAPNSSandboxChannelResponse_ =
    DeleteApnsSandboxChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aPNSSandboxChannelResponse =
          pAPNSSandboxChannelResponse_
      }

-- | The response's http status code.
deleteApnsSandboxChannelResponse_httpStatus :: Lens.Lens' DeleteApnsSandboxChannelResponse Prelude.Int
deleteApnsSandboxChannelResponse_httpStatus = Lens.lens (\DeleteApnsSandboxChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteApnsSandboxChannelResponse' {} a -> s {httpStatus = a} :: DeleteApnsSandboxChannelResponse)

-- | Undocumented member.
deleteApnsSandboxChannelResponse_aPNSSandboxChannelResponse :: Lens.Lens' DeleteApnsSandboxChannelResponse APNSSandboxChannelResponse
deleteApnsSandboxChannelResponse_aPNSSandboxChannelResponse = Lens.lens (\DeleteApnsSandboxChannelResponse' {aPNSSandboxChannelResponse} -> aPNSSandboxChannelResponse) (\s@DeleteApnsSandboxChannelResponse' {} a -> s {aPNSSandboxChannelResponse = a} :: DeleteApnsSandboxChannelResponse)

instance
  Prelude.NFData
    DeleteApnsSandboxChannelResponse
  where
  rnf DeleteApnsSandboxChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf aPNSSandboxChannelResponse
