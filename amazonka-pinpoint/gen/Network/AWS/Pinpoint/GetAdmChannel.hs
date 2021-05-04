{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.GetAdmChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the ADM channel
-- for an application.
module Network.AWS.Pinpoint.GetAdmChannel
  ( -- * Creating a Request
    GetAdmChannel (..),
    newGetAdmChannel,

    -- * Request Lenses
    getAdmChannel_applicationId,

    -- * Destructuring the Response
    GetAdmChannelResponse (..),
    newGetAdmChannelResponse,

    -- * Response Lenses
    getAdmChannelResponse_httpStatus,
    getAdmChannelResponse_aDMChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAdmChannel' smart constructor.
data GetAdmChannel = GetAdmChannel'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetAdmChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getAdmChannel_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetAdmChannel ::
  -- | 'applicationId'
  Prelude.Text ->
  GetAdmChannel
newGetAdmChannel pApplicationId_ =
  GetAdmChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getAdmChannel_applicationId :: Lens.Lens' GetAdmChannel Prelude.Text
getAdmChannel_applicationId = Lens.lens (\GetAdmChannel' {applicationId} -> applicationId) (\s@GetAdmChannel' {} a -> s {applicationId = a} :: GetAdmChannel)

instance Prelude.AWSRequest GetAdmChannel where
  type Rs GetAdmChannel = GetAdmChannelResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAdmChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.eitherParseJSON x)
      )

instance Prelude.Hashable GetAdmChannel

instance Prelude.NFData GetAdmChannel

instance Prelude.ToHeaders GetAdmChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath GetAdmChannel where
  toPath GetAdmChannel' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Prelude.toBS applicationId,
        "/channels/adm"
      ]

instance Prelude.ToQuery GetAdmChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAdmChannelResponse' smart constructor.
data GetAdmChannelResponse = GetAdmChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    aDMChannelResponse :: ADMChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetAdmChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getAdmChannelResponse_httpStatus' - The response's http status code.
--
-- 'aDMChannelResponse', 'getAdmChannelResponse_aDMChannelResponse' - Undocumented member.
newGetAdmChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'aDMChannelResponse'
  ADMChannelResponse ->
  GetAdmChannelResponse
newGetAdmChannelResponse
  pHttpStatus_
  pADMChannelResponse_ =
    GetAdmChannelResponse'
      { httpStatus = pHttpStatus_,
        aDMChannelResponse = pADMChannelResponse_
      }

-- | The response's http status code.
getAdmChannelResponse_httpStatus :: Lens.Lens' GetAdmChannelResponse Prelude.Int
getAdmChannelResponse_httpStatus = Lens.lens (\GetAdmChannelResponse' {httpStatus} -> httpStatus) (\s@GetAdmChannelResponse' {} a -> s {httpStatus = a} :: GetAdmChannelResponse)

-- | Undocumented member.
getAdmChannelResponse_aDMChannelResponse :: Lens.Lens' GetAdmChannelResponse ADMChannelResponse
getAdmChannelResponse_aDMChannelResponse = Lens.lens (\GetAdmChannelResponse' {aDMChannelResponse} -> aDMChannelResponse) (\s@GetAdmChannelResponse' {} a -> s {aDMChannelResponse = a} :: GetAdmChannelResponse)

instance Prelude.NFData GetAdmChannelResponse
