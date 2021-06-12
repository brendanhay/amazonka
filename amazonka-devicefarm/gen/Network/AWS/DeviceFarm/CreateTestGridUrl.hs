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
-- Module      : Network.AWS.DeviceFarm.CreateTestGridUrl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a signed, short-term URL that can be passed to a Selenium
-- @RemoteWebDriver@ constructor.
module Network.AWS.DeviceFarm.CreateTestGridUrl
  ( -- * Creating a Request
    CreateTestGridUrl (..),
    newCreateTestGridUrl,

    -- * Request Lenses
    createTestGridUrl_projectArn,
    createTestGridUrl_expiresInSeconds,

    -- * Destructuring the Response
    CreateTestGridUrlResponse (..),
    newCreateTestGridUrlResponse,

    -- * Response Lenses
    createTestGridUrlResponse_url,
    createTestGridUrlResponse_expires,
    createTestGridUrlResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTestGridUrl' smart constructor.
data CreateTestGridUrl = CreateTestGridUrl'
  { -- | ARN (from CreateTestGridProject or ListTestGridProjects) to associate
    -- with the short-term URL.
    projectArn :: Core.Text,
    -- | Lifetime, in seconds, of the URL.
    expiresInSeconds :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTestGridUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectArn', 'createTestGridUrl_projectArn' - ARN (from CreateTestGridProject or ListTestGridProjects) to associate
-- with the short-term URL.
--
-- 'expiresInSeconds', 'createTestGridUrl_expiresInSeconds' - Lifetime, in seconds, of the URL.
newCreateTestGridUrl ::
  -- | 'projectArn'
  Core.Text ->
  -- | 'expiresInSeconds'
  Core.Natural ->
  CreateTestGridUrl
newCreateTestGridUrl pProjectArn_ pExpiresInSeconds_ =
  CreateTestGridUrl'
    { projectArn = pProjectArn_,
      expiresInSeconds = pExpiresInSeconds_
    }

-- | ARN (from CreateTestGridProject or ListTestGridProjects) to associate
-- with the short-term URL.
createTestGridUrl_projectArn :: Lens.Lens' CreateTestGridUrl Core.Text
createTestGridUrl_projectArn = Lens.lens (\CreateTestGridUrl' {projectArn} -> projectArn) (\s@CreateTestGridUrl' {} a -> s {projectArn = a} :: CreateTestGridUrl)

-- | Lifetime, in seconds, of the URL.
createTestGridUrl_expiresInSeconds :: Lens.Lens' CreateTestGridUrl Core.Natural
createTestGridUrl_expiresInSeconds = Lens.lens (\CreateTestGridUrl' {expiresInSeconds} -> expiresInSeconds) (\s@CreateTestGridUrl' {} a -> s {expiresInSeconds = a} :: CreateTestGridUrl)

instance Core.AWSRequest CreateTestGridUrl where
  type
    AWSResponse CreateTestGridUrl =
      CreateTestGridUrlResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTestGridUrlResponse'
            Core.<$> (x Core..?> "url")
            Core.<*> (x Core..?> "expires")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateTestGridUrl

instance Core.NFData CreateTestGridUrl

instance Core.ToHeaders CreateTestGridUrl where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.CreateTestGridUrl" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateTestGridUrl where
  toJSON CreateTestGridUrl' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("projectArn" Core..= projectArn),
            Core.Just
              ("expiresInSeconds" Core..= expiresInSeconds)
          ]
      )

instance Core.ToPath CreateTestGridUrl where
  toPath = Core.const "/"

instance Core.ToQuery CreateTestGridUrl where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateTestGridUrlResponse' smart constructor.
data CreateTestGridUrlResponse = CreateTestGridUrlResponse'
  { -- | A signed URL, expiring in CreateTestGridUrlRequest$expiresInSeconds
    -- seconds, to be passed to a @RemoteWebDriver@.
    url :: Core.Maybe Core.Text,
    -- | The number of seconds the URL from CreateTestGridUrlResult$url stays
    -- active.
    expires :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTestGridUrlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'createTestGridUrlResponse_url' - A signed URL, expiring in CreateTestGridUrlRequest$expiresInSeconds
-- seconds, to be passed to a @RemoteWebDriver@.
--
-- 'expires', 'createTestGridUrlResponse_expires' - The number of seconds the URL from CreateTestGridUrlResult$url stays
-- active.
--
-- 'httpStatus', 'createTestGridUrlResponse_httpStatus' - The response's http status code.
newCreateTestGridUrlResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateTestGridUrlResponse
newCreateTestGridUrlResponse pHttpStatus_ =
  CreateTestGridUrlResponse'
    { url = Core.Nothing,
      expires = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A signed URL, expiring in CreateTestGridUrlRequest$expiresInSeconds
-- seconds, to be passed to a @RemoteWebDriver@.
createTestGridUrlResponse_url :: Lens.Lens' CreateTestGridUrlResponse (Core.Maybe Core.Text)
createTestGridUrlResponse_url = Lens.lens (\CreateTestGridUrlResponse' {url} -> url) (\s@CreateTestGridUrlResponse' {} a -> s {url = a} :: CreateTestGridUrlResponse)

-- | The number of seconds the URL from CreateTestGridUrlResult$url stays
-- active.
createTestGridUrlResponse_expires :: Lens.Lens' CreateTestGridUrlResponse (Core.Maybe Core.UTCTime)
createTestGridUrlResponse_expires = Lens.lens (\CreateTestGridUrlResponse' {expires} -> expires) (\s@CreateTestGridUrlResponse' {} a -> s {expires = a} :: CreateTestGridUrlResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
createTestGridUrlResponse_httpStatus :: Lens.Lens' CreateTestGridUrlResponse Core.Int
createTestGridUrlResponse_httpStatus = Lens.lens (\CreateTestGridUrlResponse' {httpStatus} -> httpStatus) (\s@CreateTestGridUrlResponse' {} a -> s {httpStatus = a} :: CreateTestGridUrlResponse)

instance Core.NFData CreateTestGridUrlResponse
