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
-- Module      : Amazonka.Pinpoint.DeleteSmsChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the SMS channel for an application and deletes any existing
-- settings for the channel.
module Amazonka.Pinpoint.DeleteSmsChannel
  ( -- * Creating a Request
    DeleteSmsChannel (..),
    newDeleteSmsChannel,

    -- * Request Lenses
    deleteSmsChannel_applicationId,

    -- * Destructuring the Response
    DeleteSmsChannelResponse (..),
    newDeleteSmsChannelResponse,

    -- * Response Lenses
    deleteSmsChannelResponse_httpStatus,
    deleteSmsChannelResponse_sMSChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSmsChannel' smart constructor.
data DeleteSmsChannel = DeleteSmsChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSmsChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteSmsChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteSmsChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  DeleteSmsChannel
newDeleteSmsChannel pApplicationId_ =
  DeleteSmsChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteSmsChannel_applicationId :: Lens.Lens' DeleteSmsChannel Prelude.Text
deleteSmsChannel_applicationId = Lens.lens (\DeleteSmsChannel' {applicationId} -> applicationId) (\s@DeleteSmsChannel' {} a -> s {applicationId = a} :: DeleteSmsChannel)

instance Core.AWSRequest DeleteSmsChannel where
  type
    AWSResponse DeleteSmsChannel =
      DeleteSmsChannelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSmsChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteSmsChannel where
  hashWithSalt _salt DeleteSmsChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData DeleteSmsChannel where
  rnf DeleteSmsChannel' {..} = Prelude.rnf applicationId

instance Data.ToHeaders DeleteSmsChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSmsChannel where
  toPath DeleteSmsChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/channels/sms"
      ]

instance Data.ToQuery DeleteSmsChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSmsChannelResponse' smart constructor.
data DeleteSmsChannelResponse = DeleteSmsChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    sMSChannelResponse :: SMSChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSmsChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSmsChannelResponse_httpStatus' - The response's http status code.
--
-- 'sMSChannelResponse', 'deleteSmsChannelResponse_sMSChannelResponse' - Undocumented member.
newDeleteSmsChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'sMSChannelResponse'
  SMSChannelResponse ->
  DeleteSmsChannelResponse
newDeleteSmsChannelResponse
  pHttpStatus_
  pSMSChannelResponse_ =
    DeleteSmsChannelResponse'
      { httpStatus =
          pHttpStatus_,
        sMSChannelResponse = pSMSChannelResponse_
      }

-- | The response's http status code.
deleteSmsChannelResponse_httpStatus :: Lens.Lens' DeleteSmsChannelResponse Prelude.Int
deleteSmsChannelResponse_httpStatus = Lens.lens (\DeleteSmsChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteSmsChannelResponse' {} a -> s {httpStatus = a} :: DeleteSmsChannelResponse)

-- | Undocumented member.
deleteSmsChannelResponse_sMSChannelResponse :: Lens.Lens' DeleteSmsChannelResponse SMSChannelResponse
deleteSmsChannelResponse_sMSChannelResponse = Lens.lens (\DeleteSmsChannelResponse' {sMSChannelResponse} -> sMSChannelResponse) (\s@DeleteSmsChannelResponse' {} a -> s {sMSChannelResponse = a} :: DeleteSmsChannelResponse)

instance Prelude.NFData DeleteSmsChannelResponse where
  rnf DeleteSmsChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf sMSChannelResponse
