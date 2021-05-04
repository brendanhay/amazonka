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
-- Module      : Network.AWS.Pinpoint.GetChannels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the history and status of each channel for
-- an application.
module Network.AWS.Pinpoint.GetChannels
  ( -- * Creating a Request
    GetChannels (..),
    newGetChannels,

    -- * Request Lenses
    getChannels_applicationId,

    -- * Destructuring the Response
    GetChannelsResponse (..),
    newGetChannelsResponse,

    -- * Response Lenses
    getChannelsResponse_httpStatus,
    getChannelsResponse_channelsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetChannels' smart constructor.
data GetChannels = GetChannels'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetChannels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getChannels_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetChannels ::
  -- | 'applicationId'
  Prelude.Text ->
  GetChannels
newGetChannels pApplicationId_ =
  GetChannels' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getChannels_applicationId :: Lens.Lens' GetChannels Prelude.Text
getChannels_applicationId = Lens.lens (\GetChannels' {applicationId} -> applicationId) (\s@GetChannels' {} a -> s {applicationId = a} :: GetChannels)

instance Prelude.AWSRequest GetChannels where
  type Rs GetChannels = GetChannelsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChannelsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.eitherParseJSON x)
      )

instance Prelude.Hashable GetChannels

instance Prelude.NFData GetChannels

instance Prelude.ToHeaders GetChannels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath GetChannels where
  toPath GetChannels' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Prelude.toBS applicationId,
        "/channels"
      ]

instance Prelude.ToQuery GetChannels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetChannelsResponse' smart constructor.
data GetChannelsResponse = GetChannelsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    channelsResponse :: ChannelsResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetChannelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getChannelsResponse_httpStatus' - The response's http status code.
--
-- 'channelsResponse', 'getChannelsResponse_channelsResponse' - Undocumented member.
newGetChannelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'channelsResponse'
  ChannelsResponse ->
  GetChannelsResponse
newGetChannelsResponse
  pHttpStatus_
  pChannelsResponse_ =
    GetChannelsResponse'
      { httpStatus = pHttpStatus_,
        channelsResponse = pChannelsResponse_
      }

-- | The response's http status code.
getChannelsResponse_httpStatus :: Lens.Lens' GetChannelsResponse Prelude.Int
getChannelsResponse_httpStatus = Lens.lens (\GetChannelsResponse' {httpStatus} -> httpStatus) (\s@GetChannelsResponse' {} a -> s {httpStatus = a} :: GetChannelsResponse)

-- | Undocumented member.
getChannelsResponse_channelsResponse :: Lens.Lens' GetChannelsResponse ChannelsResponse
getChannelsResponse_channelsResponse = Lens.lens (\GetChannelsResponse' {channelsResponse} -> channelsResponse) (\s@GetChannelsResponse' {} a -> s {channelsResponse = a} :: GetChannelsResponse)

instance Prelude.NFData GetChannelsResponse
