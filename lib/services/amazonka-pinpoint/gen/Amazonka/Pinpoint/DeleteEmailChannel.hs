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
-- Module      : Amazonka.Pinpoint.DeleteEmailChannel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the email channel for an application and deletes any existing
-- settings for the channel.
module Amazonka.Pinpoint.DeleteEmailChannel
  ( -- * Creating a Request
    DeleteEmailChannel (..),
    newDeleteEmailChannel,

    -- * Request Lenses
    deleteEmailChannel_applicationId,

    -- * Destructuring the Response
    DeleteEmailChannelResponse (..),
    newDeleteEmailChannelResponse,

    -- * Response Lenses
    deleteEmailChannelResponse_httpStatus,
    deleteEmailChannelResponse_emailChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEmailChannel' smart constructor.
data DeleteEmailChannel = DeleteEmailChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEmailChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteEmailChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteEmailChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  DeleteEmailChannel
newDeleteEmailChannel pApplicationId_ =
  DeleteEmailChannel'
    { applicationId =
        pApplicationId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteEmailChannel_applicationId :: Lens.Lens' DeleteEmailChannel Prelude.Text
deleteEmailChannel_applicationId = Lens.lens (\DeleteEmailChannel' {applicationId} -> applicationId) (\s@DeleteEmailChannel' {} a -> s {applicationId = a} :: DeleteEmailChannel)

instance Core.AWSRequest DeleteEmailChannel where
  type
    AWSResponse DeleteEmailChannel =
      DeleteEmailChannelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEmailChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteEmailChannel where
  hashWithSalt _salt DeleteEmailChannel' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData DeleteEmailChannel where
  rnf DeleteEmailChannel' {..} =
    Prelude.rnf applicationId

instance Core.ToHeaders DeleteEmailChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteEmailChannel where
  toPath DeleteEmailChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/email"
      ]

instance Core.ToQuery DeleteEmailChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEmailChannelResponse' smart constructor.
data DeleteEmailChannelResponse = DeleteEmailChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    emailChannelResponse :: EmailChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEmailChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEmailChannelResponse_httpStatus' - The response's http status code.
--
-- 'emailChannelResponse', 'deleteEmailChannelResponse_emailChannelResponse' - Undocumented member.
newDeleteEmailChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'emailChannelResponse'
  EmailChannelResponse ->
  DeleteEmailChannelResponse
newDeleteEmailChannelResponse
  pHttpStatus_
  pEmailChannelResponse_ =
    DeleteEmailChannelResponse'
      { httpStatus =
          pHttpStatus_,
        emailChannelResponse = pEmailChannelResponse_
      }

-- | The response's http status code.
deleteEmailChannelResponse_httpStatus :: Lens.Lens' DeleteEmailChannelResponse Prelude.Int
deleteEmailChannelResponse_httpStatus = Lens.lens (\DeleteEmailChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteEmailChannelResponse' {} a -> s {httpStatus = a} :: DeleteEmailChannelResponse)

-- | Undocumented member.
deleteEmailChannelResponse_emailChannelResponse :: Lens.Lens' DeleteEmailChannelResponse EmailChannelResponse
deleteEmailChannelResponse_emailChannelResponse = Lens.lens (\DeleteEmailChannelResponse' {emailChannelResponse} -> emailChannelResponse) (\s@DeleteEmailChannelResponse' {} a -> s {emailChannelResponse = a} :: DeleteEmailChannelResponse)

instance Prelude.NFData DeleteEmailChannelResponse where
  rnf DeleteEmailChannelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf emailChannelResponse
