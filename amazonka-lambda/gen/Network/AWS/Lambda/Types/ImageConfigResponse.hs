{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.ImageConfigResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.ImageConfigResponse where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types.ImageConfig
import Network.AWS.Lambda.Types.ImageConfigError
import qualified Network.AWS.Lens as Lens

-- | Response to GetFunctionConfiguration request.
--
-- /See:/ 'newImageConfigResponse' smart constructor.
data ImageConfigResponse = ImageConfigResponse'
  { -- | Configuration values that override the container image Dockerfile.
    imageConfig :: Core.Maybe ImageConfig,
    -- | Error response to GetFunctionConfiguration.
    error :: Core.Maybe ImageConfigError
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImageConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageConfig', 'imageConfigResponse_imageConfig' - Configuration values that override the container image Dockerfile.
--
-- 'error', 'imageConfigResponse_error' - Error response to GetFunctionConfiguration.
newImageConfigResponse ::
  ImageConfigResponse
newImageConfigResponse =
  ImageConfigResponse'
    { imageConfig = Core.Nothing,
      error = Core.Nothing
    }

-- | Configuration values that override the container image Dockerfile.
imageConfigResponse_imageConfig :: Lens.Lens' ImageConfigResponse (Core.Maybe ImageConfig)
imageConfigResponse_imageConfig = Lens.lens (\ImageConfigResponse' {imageConfig} -> imageConfig) (\s@ImageConfigResponse' {} a -> s {imageConfig = a} :: ImageConfigResponse)

-- | Error response to GetFunctionConfiguration.
imageConfigResponse_error :: Lens.Lens' ImageConfigResponse (Core.Maybe ImageConfigError)
imageConfigResponse_error = Lens.lens (\ImageConfigResponse' {error} -> error) (\s@ImageConfigResponse' {} a -> s {error = a} :: ImageConfigResponse)

instance Core.FromJSON ImageConfigResponse where
  parseJSON =
    Core.withObject
      "ImageConfigResponse"
      ( \x ->
          ImageConfigResponse'
            Core.<$> (x Core..:? "ImageConfig")
            Core.<*> (x Core..:? "Error")
      )

instance Core.Hashable ImageConfigResponse

instance Core.NFData ImageConfigResponse
