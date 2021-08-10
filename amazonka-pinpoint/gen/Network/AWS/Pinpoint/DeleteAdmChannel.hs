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
-- Module      : Network.AWS.Pinpoint.DeleteAdmChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the ADM channel for an application and deletes any existing
-- settings for the channel.
module Network.AWS.Pinpoint.DeleteAdmChannel
  ( -- * Creating a Request
    DeleteAdmChannel (..),
    newDeleteAdmChannel,

    -- * Request Lenses
    deleteAdmChannel_applicationId,

    -- * Destructuring the Response
    DeleteAdmChannelResponse (..),
    newDeleteAdmChannelResponse,

    -- * Response Lenses
    deleteAdmChannelResponse_httpStatus,
    deleteAdmChannelResponse_aDMChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAdmChannel' smart constructor.
data DeleteAdmChannel = DeleteAdmChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAdmChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteAdmChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteAdmChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  DeleteAdmChannel
newDeleteAdmChannel pApplicationId_ =
  DeleteAdmChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteAdmChannel_applicationId :: Lens.Lens' DeleteAdmChannel Prelude.Text
deleteAdmChannel_applicationId = Lens.lens (\DeleteAdmChannel' {applicationId} -> applicationId) (\s@DeleteAdmChannel' {} a -> s {applicationId = a} :: DeleteAdmChannel)

instance Core.AWSRequest DeleteAdmChannel where
  type
    AWSResponse DeleteAdmChannel =
      DeleteAdmChannelResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAdmChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteAdmChannel

instance Prelude.NFData DeleteAdmChannel

instance Core.ToHeaders DeleteAdmChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteAdmChannel where
  toPath DeleteAdmChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/channels/adm"
      ]

instance Core.ToQuery DeleteAdmChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAdmChannelResponse' smart constructor.
data DeleteAdmChannelResponse = DeleteAdmChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    aDMChannelResponse :: ADMChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAdmChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAdmChannelResponse_httpStatus' - The response's http status code.
--
-- 'aDMChannelResponse', 'deleteAdmChannelResponse_aDMChannelResponse' - Undocumented member.
newDeleteAdmChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'aDMChannelResponse'
  ADMChannelResponse ->
  DeleteAdmChannelResponse
newDeleteAdmChannelResponse
  pHttpStatus_
  pADMChannelResponse_ =
    DeleteAdmChannelResponse'
      { httpStatus =
          pHttpStatus_,
        aDMChannelResponse = pADMChannelResponse_
      }

-- | The response's http status code.
deleteAdmChannelResponse_httpStatus :: Lens.Lens' DeleteAdmChannelResponse Prelude.Int
deleteAdmChannelResponse_httpStatus = Lens.lens (\DeleteAdmChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteAdmChannelResponse' {} a -> s {httpStatus = a} :: DeleteAdmChannelResponse)

-- | Undocumented member.
deleteAdmChannelResponse_aDMChannelResponse :: Lens.Lens' DeleteAdmChannelResponse ADMChannelResponse
deleteAdmChannelResponse_aDMChannelResponse = Lens.lens (\DeleteAdmChannelResponse' {aDMChannelResponse} -> aDMChannelResponse) (\s@DeleteAdmChannelResponse' {} a -> s {aDMChannelResponse = a} :: DeleteAdmChannelResponse)

instance Prelude.NFData DeleteAdmChannelResponse
