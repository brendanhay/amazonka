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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.ImageConfigResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.ImageConfig
import Amazonka.Lambda.Types.ImageConfigError
import qualified Amazonka.Prelude as Prelude

-- | Response to a @GetFunctionConfiguration@ request.
--
-- /See:/ 'newImageConfigResponse' smart constructor.
data ImageConfigResponse = ImageConfigResponse'
  { -- | Error response to @GetFunctionConfiguration@.
    error :: Prelude.Maybe ImageConfigError,
    -- | Configuration values that override the container image Dockerfile.
    imageConfig :: Prelude.Maybe ImageConfig
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
-- 'error', 'imageConfigResponse_error' - Error response to @GetFunctionConfiguration@.
--
-- 'imageConfig', 'imageConfigResponse_imageConfig' - Configuration values that override the container image Dockerfile.
newImageConfigResponse ::
  ImageConfigResponse
newImageConfigResponse =
  ImageConfigResponse'
    { error = Prelude.Nothing,
      imageConfig = Prelude.Nothing
    }

-- | Error response to @GetFunctionConfiguration@.
imageConfigResponse_error :: Lens.Lens' ImageConfigResponse (Prelude.Maybe ImageConfigError)
imageConfigResponse_error = Lens.lens (\ImageConfigResponse' {error} -> error) (\s@ImageConfigResponse' {} a -> s {error = a} :: ImageConfigResponse)

-- | Configuration values that override the container image Dockerfile.
imageConfigResponse_imageConfig :: Lens.Lens' ImageConfigResponse (Prelude.Maybe ImageConfig)
imageConfigResponse_imageConfig = Lens.lens (\ImageConfigResponse' {imageConfig} -> imageConfig) (\s@ImageConfigResponse' {} a -> s {imageConfig = a} :: ImageConfigResponse)

instance Data.FromJSON ImageConfigResponse where
  parseJSON =
    Data.withObject
      "ImageConfigResponse"
      ( \x ->
          ImageConfigResponse'
            Prelude.<$> (x Data..:? "Error")
            Prelude.<*> (x Data..:? "ImageConfig")
      )

instance Prelude.Hashable ImageConfigResponse where
  hashWithSalt _salt ImageConfigResponse' {..} =
    _salt
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` imageConfig

instance Prelude.NFData ImageConfigResponse where
  rnf ImageConfigResponse' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf imageConfig
