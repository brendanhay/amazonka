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
-- Module      : Network.AWS.AppStream.CreateImageBuilderStreamingURL
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a URL to start an image builder streaming session.
module Network.AWS.AppStream.CreateImageBuilderStreamingURL
  ( -- * Creating a Request
    CreateImageBuilderStreamingURL (..),
    newCreateImageBuilderStreamingURL,

    -- * Request Lenses
    createImageBuilderStreamingURL_validity,
    createImageBuilderStreamingURL_name,

    -- * Destructuring the Response
    CreateImageBuilderStreamingURLResponse (..),
    newCreateImageBuilderStreamingURLResponse,

    -- * Response Lenses
    createImageBuilderStreamingURLResponse_streamingURL,
    createImageBuilderStreamingURLResponse_expires,
    createImageBuilderStreamingURLResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateImageBuilderStreamingURL' smart constructor.
data CreateImageBuilderStreamingURL = CreateImageBuilderStreamingURL'
  { -- | The time that the streaming URL will be valid, in seconds. Specify a
    -- value between 1 and 604800 seconds. The default is 3600 seconds.
    validity :: Core.Maybe Core.Integer,
    -- | The name of the image builder.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateImageBuilderStreamingURL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validity', 'createImageBuilderStreamingURL_validity' - The time that the streaming URL will be valid, in seconds. Specify a
-- value between 1 and 604800 seconds. The default is 3600 seconds.
--
-- 'name', 'createImageBuilderStreamingURL_name' - The name of the image builder.
newCreateImageBuilderStreamingURL ::
  -- | 'name'
  Core.Text ->
  CreateImageBuilderStreamingURL
newCreateImageBuilderStreamingURL pName_ =
  CreateImageBuilderStreamingURL'
    { validity =
        Core.Nothing,
      name = pName_
    }

-- | The time that the streaming URL will be valid, in seconds. Specify a
-- value between 1 and 604800 seconds. The default is 3600 seconds.
createImageBuilderStreamingURL_validity :: Lens.Lens' CreateImageBuilderStreamingURL (Core.Maybe Core.Integer)
createImageBuilderStreamingURL_validity = Lens.lens (\CreateImageBuilderStreamingURL' {validity} -> validity) (\s@CreateImageBuilderStreamingURL' {} a -> s {validity = a} :: CreateImageBuilderStreamingURL)

-- | The name of the image builder.
createImageBuilderStreamingURL_name :: Lens.Lens' CreateImageBuilderStreamingURL Core.Text
createImageBuilderStreamingURL_name = Lens.lens (\CreateImageBuilderStreamingURL' {name} -> name) (\s@CreateImageBuilderStreamingURL' {} a -> s {name = a} :: CreateImageBuilderStreamingURL)

instance
  Core.AWSRequest
    CreateImageBuilderStreamingURL
  where
  type
    AWSResponse CreateImageBuilderStreamingURL =
      CreateImageBuilderStreamingURLResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateImageBuilderStreamingURLResponse'
            Core.<$> (x Core..?> "StreamingURL")
            Core.<*> (x Core..?> "Expires")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateImageBuilderStreamingURL

instance Core.NFData CreateImageBuilderStreamingURL

instance
  Core.ToHeaders
    CreateImageBuilderStreamingURL
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.CreateImageBuilderStreamingURL" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateImageBuilderStreamingURL where
  toJSON CreateImageBuilderStreamingURL' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Validity" Core..=) Core.<$> validity,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateImageBuilderStreamingURL where
  toPath = Core.const "/"

instance Core.ToQuery CreateImageBuilderStreamingURL where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateImageBuilderStreamingURLResponse' smart constructor.
data CreateImageBuilderStreamingURLResponse = CreateImageBuilderStreamingURLResponse'
  { -- | The URL to start the AppStream 2.0 streaming session.
    streamingURL :: Core.Maybe Core.Text,
    -- | The elapsed time, in seconds after the Unix epoch, when this URL
    -- expires.
    expires :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateImageBuilderStreamingURLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingURL', 'createImageBuilderStreamingURLResponse_streamingURL' - The URL to start the AppStream 2.0 streaming session.
--
-- 'expires', 'createImageBuilderStreamingURLResponse_expires' - The elapsed time, in seconds after the Unix epoch, when this URL
-- expires.
--
-- 'httpStatus', 'createImageBuilderStreamingURLResponse_httpStatus' - The response's http status code.
newCreateImageBuilderStreamingURLResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateImageBuilderStreamingURLResponse
newCreateImageBuilderStreamingURLResponse
  pHttpStatus_ =
    CreateImageBuilderStreamingURLResponse'
      { streamingURL =
          Core.Nothing,
        expires = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The URL to start the AppStream 2.0 streaming session.
createImageBuilderStreamingURLResponse_streamingURL :: Lens.Lens' CreateImageBuilderStreamingURLResponse (Core.Maybe Core.Text)
createImageBuilderStreamingURLResponse_streamingURL = Lens.lens (\CreateImageBuilderStreamingURLResponse' {streamingURL} -> streamingURL) (\s@CreateImageBuilderStreamingURLResponse' {} a -> s {streamingURL = a} :: CreateImageBuilderStreamingURLResponse)

-- | The elapsed time, in seconds after the Unix epoch, when this URL
-- expires.
createImageBuilderStreamingURLResponse_expires :: Lens.Lens' CreateImageBuilderStreamingURLResponse (Core.Maybe Core.UTCTime)
createImageBuilderStreamingURLResponse_expires = Lens.lens (\CreateImageBuilderStreamingURLResponse' {expires} -> expires) (\s@CreateImageBuilderStreamingURLResponse' {} a -> s {expires = a} :: CreateImageBuilderStreamingURLResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
createImageBuilderStreamingURLResponse_httpStatus :: Lens.Lens' CreateImageBuilderStreamingURLResponse Core.Int
createImageBuilderStreamingURLResponse_httpStatus = Lens.lens (\CreateImageBuilderStreamingURLResponse' {httpStatus} -> httpStatus) (\s@CreateImageBuilderStreamingURLResponse' {} a -> s {httpStatus = a} :: CreateImageBuilderStreamingURLResponse)

instance
  Core.NFData
    CreateImageBuilderStreamingURLResponse
