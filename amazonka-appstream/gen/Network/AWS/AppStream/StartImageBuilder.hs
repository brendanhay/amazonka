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
-- Module      : Network.AWS.AppStream.StartImageBuilder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified image builder.
module Network.AWS.AppStream.StartImageBuilder
  ( -- * Creating a Request
    StartImageBuilder (..),
    newStartImageBuilder,

    -- * Request Lenses
    startImageBuilder_appstreamAgentVersion,
    startImageBuilder_name,

    -- * Destructuring the Response
    StartImageBuilderResponse (..),
    newStartImageBuilderResponse,

    -- * Response Lenses
    startImageBuilderResponse_imageBuilder,
    startImageBuilderResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartImageBuilder' smart constructor.
data StartImageBuilder = StartImageBuilder'
  { -- | The version of the AppStream 2.0 agent to use for this image builder. To
    -- use the latest version of the AppStream 2.0 agent, specify [LATEST].
    appstreamAgentVersion :: Core.Maybe Core.Text,
    -- | The name of the image builder.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartImageBuilder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appstreamAgentVersion', 'startImageBuilder_appstreamAgentVersion' - The version of the AppStream 2.0 agent to use for this image builder. To
-- use the latest version of the AppStream 2.0 agent, specify [LATEST].
--
-- 'name', 'startImageBuilder_name' - The name of the image builder.
newStartImageBuilder ::
  -- | 'name'
  Core.Text ->
  StartImageBuilder
newStartImageBuilder pName_ =
  StartImageBuilder'
    { appstreamAgentVersion =
        Core.Nothing,
      name = pName_
    }

-- | The version of the AppStream 2.0 agent to use for this image builder. To
-- use the latest version of the AppStream 2.0 agent, specify [LATEST].
startImageBuilder_appstreamAgentVersion :: Lens.Lens' StartImageBuilder (Core.Maybe Core.Text)
startImageBuilder_appstreamAgentVersion = Lens.lens (\StartImageBuilder' {appstreamAgentVersion} -> appstreamAgentVersion) (\s@StartImageBuilder' {} a -> s {appstreamAgentVersion = a} :: StartImageBuilder)

-- | The name of the image builder.
startImageBuilder_name :: Lens.Lens' StartImageBuilder Core.Text
startImageBuilder_name = Lens.lens (\StartImageBuilder' {name} -> name) (\s@StartImageBuilder' {} a -> s {name = a} :: StartImageBuilder)

instance Core.AWSRequest StartImageBuilder where
  type
    AWSResponse StartImageBuilder =
      StartImageBuilderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartImageBuilderResponse'
            Core.<$> (x Core..?> "ImageBuilder")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartImageBuilder

instance Core.NFData StartImageBuilder

instance Core.ToHeaders StartImageBuilder where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.StartImageBuilder" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartImageBuilder where
  toJSON StartImageBuilder' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AppstreamAgentVersion" Core..=)
              Core.<$> appstreamAgentVersion,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath StartImageBuilder where
  toPath = Core.const "/"

instance Core.ToQuery StartImageBuilder where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartImageBuilderResponse' smart constructor.
data StartImageBuilderResponse = StartImageBuilderResponse'
  { -- | Information about the image builder.
    imageBuilder :: Core.Maybe ImageBuilder,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartImageBuilderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageBuilder', 'startImageBuilderResponse_imageBuilder' - Information about the image builder.
--
-- 'httpStatus', 'startImageBuilderResponse_httpStatus' - The response's http status code.
newStartImageBuilderResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartImageBuilderResponse
newStartImageBuilderResponse pHttpStatus_ =
  StartImageBuilderResponse'
    { imageBuilder =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the image builder.
startImageBuilderResponse_imageBuilder :: Lens.Lens' StartImageBuilderResponse (Core.Maybe ImageBuilder)
startImageBuilderResponse_imageBuilder = Lens.lens (\StartImageBuilderResponse' {imageBuilder} -> imageBuilder) (\s@StartImageBuilderResponse' {} a -> s {imageBuilder = a} :: StartImageBuilderResponse)

-- | The response's http status code.
startImageBuilderResponse_httpStatus :: Lens.Lens' StartImageBuilderResponse Core.Int
startImageBuilderResponse_httpStatus = Lens.lens (\StartImageBuilderResponse' {httpStatus} -> httpStatus) (\s@StartImageBuilderResponse' {} a -> s {httpStatus = a} :: StartImageBuilderResponse)

instance Core.NFData StartImageBuilderResponse
