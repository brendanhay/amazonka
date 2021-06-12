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
-- Module      : Network.AWS.AppStream.StopImageBuilder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified image builder.
module Network.AWS.AppStream.StopImageBuilder
  ( -- * Creating a Request
    StopImageBuilder (..),
    newStopImageBuilder,

    -- * Request Lenses
    stopImageBuilder_name,

    -- * Destructuring the Response
    StopImageBuilderResponse (..),
    newStopImageBuilderResponse,

    -- * Response Lenses
    stopImageBuilderResponse_imageBuilder,
    stopImageBuilderResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopImageBuilder' smart constructor.
data StopImageBuilder = StopImageBuilder'
  { -- | The name of the image builder.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopImageBuilder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stopImageBuilder_name' - The name of the image builder.
newStopImageBuilder ::
  -- | 'name'
  Core.Text ->
  StopImageBuilder
newStopImageBuilder pName_ =
  StopImageBuilder' {name = pName_}

-- | The name of the image builder.
stopImageBuilder_name :: Lens.Lens' StopImageBuilder Core.Text
stopImageBuilder_name = Lens.lens (\StopImageBuilder' {name} -> name) (\s@StopImageBuilder' {} a -> s {name = a} :: StopImageBuilder)

instance Core.AWSRequest StopImageBuilder where
  type
    AWSResponse StopImageBuilder =
      StopImageBuilderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopImageBuilderResponse'
            Core.<$> (x Core..?> "ImageBuilder")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopImageBuilder

instance Core.NFData StopImageBuilder

instance Core.ToHeaders StopImageBuilder where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.StopImageBuilder" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopImageBuilder where
  toJSON StopImageBuilder' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath StopImageBuilder where
  toPath = Core.const "/"

instance Core.ToQuery StopImageBuilder where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopImageBuilderResponse' smart constructor.
data StopImageBuilderResponse = StopImageBuilderResponse'
  { -- | Information about the image builder.
    imageBuilder :: Core.Maybe ImageBuilder,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopImageBuilderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageBuilder', 'stopImageBuilderResponse_imageBuilder' - Information about the image builder.
--
-- 'httpStatus', 'stopImageBuilderResponse_httpStatus' - The response's http status code.
newStopImageBuilderResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopImageBuilderResponse
newStopImageBuilderResponse pHttpStatus_ =
  StopImageBuilderResponse'
    { imageBuilder =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the image builder.
stopImageBuilderResponse_imageBuilder :: Lens.Lens' StopImageBuilderResponse (Core.Maybe ImageBuilder)
stopImageBuilderResponse_imageBuilder = Lens.lens (\StopImageBuilderResponse' {imageBuilder} -> imageBuilder) (\s@StopImageBuilderResponse' {} a -> s {imageBuilder = a} :: StopImageBuilderResponse)

-- | The response's http status code.
stopImageBuilderResponse_httpStatus :: Lens.Lens' StopImageBuilderResponse Core.Int
stopImageBuilderResponse_httpStatus = Lens.lens (\StopImageBuilderResponse' {httpStatus} -> httpStatus) (\s@StopImageBuilderResponse' {} a -> s {httpStatus = a} :: StopImageBuilderResponse)

instance Core.NFData StopImageBuilderResponse
