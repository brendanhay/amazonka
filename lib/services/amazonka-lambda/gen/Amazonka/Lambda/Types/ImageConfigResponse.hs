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
-- Module      : Amazonka.Lambda.Types.ImageConfigResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.ImageConfigResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lambda.Types.ImageConfig
import Amazonka.Lambda.Types.ImageConfigError
import qualified Amazonka.Prelude as Prelude

-- | Response to GetFunctionConfiguration request.
--
-- /See:/ 'newImageConfigResponse' smart constructor.
data ImageConfigResponse = ImageConfigResponse'
  { -- | Configuration values that override the container image Dockerfile.
    imageConfig :: Prelude.Maybe ImageConfig,
    -- | Error response to GetFunctionConfiguration.
    error :: Prelude.Maybe ImageConfigError
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
    { imageConfig = Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | Configuration values that override the container image Dockerfile.
imageConfigResponse_imageConfig :: Lens.Lens' ImageConfigResponse (Prelude.Maybe ImageConfig)
imageConfigResponse_imageConfig = Lens.lens (\ImageConfigResponse' {imageConfig} -> imageConfig) (\s@ImageConfigResponse' {} a -> s {imageConfig = a} :: ImageConfigResponse)

-- | Error response to GetFunctionConfiguration.
imageConfigResponse_error :: Lens.Lens' ImageConfigResponse (Prelude.Maybe ImageConfigError)
imageConfigResponse_error = Lens.lens (\ImageConfigResponse' {error} -> error) (\s@ImageConfigResponse' {} a -> s {error = a} :: ImageConfigResponse)

instance Core.FromJSON ImageConfigResponse where
  parseJSON =
    Core.withObject
      "ImageConfigResponse"
      ( \x ->
          ImageConfigResponse'
            Prelude.<$> (x Core..:? "ImageConfig")
            Prelude.<*> (x Core..:? "Error")
      )

instance Prelude.Hashable ImageConfigResponse where
  hashWithSalt _salt ImageConfigResponse' {..} =
    _salt `Prelude.hashWithSalt` imageConfig
      `Prelude.hashWithSalt` error

instance Prelude.NFData ImageConfigResponse where
  rnf ImageConfigResponse' {..} =
    Prelude.rnf imageConfig
      `Prelude.seq` Prelude.rnf error
