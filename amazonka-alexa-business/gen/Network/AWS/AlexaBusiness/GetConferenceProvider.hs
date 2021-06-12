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
-- Module      : Network.AWS.AlexaBusiness.GetConferenceProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a specific conference provider.
module Network.AWS.AlexaBusiness.GetConferenceProvider
  ( -- * Creating a Request
    GetConferenceProvider (..),
    newGetConferenceProvider,

    -- * Request Lenses
    getConferenceProvider_conferenceProviderArn,

    -- * Destructuring the Response
    GetConferenceProviderResponse (..),
    newGetConferenceProviderResponse,

    -- * Response Lenses
    getConferenceProviderResponse_conferenceProvider,
    getConferenceProviderResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetConferenceProvider' smart constructor.
data GetConferenceProvider = GetConferenceProvider'
  { -- | The ARN of the newly created conference provider.
    conferenceProviderArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConferenceProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conferenceProviderArn', 'getConferenceProvider_conferenceProviderArn' - The ARN of the newly created conference provider.
newGetConferenceProvider ::
  -- | 'conferenceProviderArn'
  Core.Text ->
  GetConferenceProvider
newGetConferenceProvider pConferenceProviderArn_ =
  GetConferenceProvider'
    { conferenceProviderArn =
        pConferenceProviderArn_
    }

-- | The ARN of the newly created conference provider.
getConferenceProvider_conferenceProviderArn :: Lens.Lens' GetConferenceProvider Core.Text
getConferenceProvider_conferenceProviderArn = Lens.lens (\GetConferenceProvider' {conferenceProviderArn} -> conferenceProviderArn) (\s@GetConferenceProvider' {} a -> s {conferenceProviderArn = a} :: GetConferenceProvider)

instance Core.AWSRequest GetConferenceProvider where
  type
    AWSResponse GetConferenceProvider =
      GetConferenceProviderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConferenceProviderResponse'
            Core.<$> (x Core..?> "ConferenceProvider")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetConferenceProvider

instance Core.NFData GetConferenceProvider

instance Core.ToHeaders GetConferenceProvider where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.GetConferenceProvider" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetConferenceProvider where
  toJSON GetConferenceProvider' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ConferenceProviderArn"
                  Core..= conferenceProviderArn
              )
          ]
      )

instance Core.ToPath GetConferenceProvider where
  toPath = Core.const "/"

instance Core.ToQuery GetConferenceProvider where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetConferenceProviderResponse' smart constructor.
data GetConferenceProviderResponse = GetConferenceProviderResponse'
  { -- | The conference provider.
    conferenceProvider :: Core.Maybe ConferenceProvider,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConferenceProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conferenceProvider', 'getConferenceProviderResponse_conferenceProvider' - The conference provider.
--
-- 'httpStatus', 'getConferenceProviderResponse_httpStatus' - The response's http status code.
newGetConferenceProviderResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetConferenceProviderResponse
newGetConferenceProviderResponse pHttpStatus_ =
  GetConferenceProviderResponse'
    { conferenceProvider =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The conference provider.
getConferenceProviderResponse_conferenceProvider :: Lens.Lens' GetConferenceProviderResponse (Core.Maybe ConferenceProvider)
getConferenceProviderResponse_conferenceProvider = Lens.lens (\GetConferenceProviderResponse' {conferenceProvider} -> conferenceProvider) (\s@GetConferenceProviderResponse' {} a -> s {conferenceProvider = a} :: GetConferenceProviderResponse)

-- | The response's http status code.
getConferenceProviderResponse_httpStatus :: Lens.Lens' GetConferenceProviderResponse Core.Int
getConferenceProviderResponse_httpStatus = Lens.lens (\GetConferenceProviderResponse' {httpStatus} -> httpStatus) (\s@GetConferenceProviderResponse' {} a -> s {httpStatus = a} :: GetConferenceProviderResponse)

instance Core.NFData GetConferenceProviderResponse
