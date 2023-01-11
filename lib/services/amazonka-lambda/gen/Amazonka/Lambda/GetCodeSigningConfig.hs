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
-- Module      : Amazonka.Lambda.GetCodeSigningConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified code signing configuration.
module Amazonka.Lambda.GetCodeSigningConfig
  ( -- * Creating a Request
    GetCodeSigningConfig (..),
    newGetCodeSigningConfig,

    -- * Request Lenses
    getCodeSigningConfig_codeSigningConfigArn,

    -- * Destructuring the Response
    GetCodeSigningConfigResponse (..),
    newGetCodeSigningConfigResponse,

    -- * Response Lenses
    getCodeSigningConfigResponse_httpStatus,
    getCodeSigningConfigResponse_codeSigningConfig,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCodeSigningConfig' smart constructor.
data GetCodeSigningConfig = GetCodeSigningConfig'
  { -- | The The Amazon Resource Name (ARN) of the code signing configuration.
    codeSigningConfigArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCodeSigningConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeSigningConfigArn', 'getCodeSigningConfig_codeSigningConfigArn' - The The Amazon Resource Name (ARN) of the code signing configuration.
newGetCodeSigningConfig ::
  -- | 'codeSigningConfigArn'
  Prelude.Text ->
  GetCodeSigningConfig
newGetCodeSigningConfig pCodeSigningConfigArn_ =
  GetCodeSigningConfig'
    { codeSigningConfigArn =
        pCodeSigningConfigArn_
    }

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
getCodeSigningConfig_codeSigningConfigArn :: Lens.Lens' GetCodeSigningConfig Prelude.Text
getCodeSigningConfig_codeSigningConfigArn = Lens.lens (\GetCodeSigningConfig' {codeSigningConfigArn} -> codeSigningConfigArn) (\s@GetCodeSigningConfig' {} a -> s {codeSigningConfigArn = a} :: GetCodeSigningConfig)

instance Core.AWSRequest GetCodeSigningConfig where
  type
    AWSResponse GetCodeSigningConfig =
      GetCodeSigningConfigResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCodeSigningConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CodeSigningConfig")
      )

instance Prelude.Hashable GetCodeSigningConfig where
  hashWithSalt _salt GetCodeSigningConfig' {..} =
    _salt `Prelude.hashWithSalt` codeSigningConfigArn

instance Prelude.NFData GetCodeSigningConfig where
  rnf GetCodeSigningConfig' {..} =
    Prelude.rnf codeSigningConfigArn

instance Data.ToHeaders GetCodeSigningConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetCodeSigningConfig where
  toPath GetCodeSigningConfig' {..} =
    Prelude.mconcat
      [ "/2020-04-22/code-signing-configs/",
        Data.toBS codeSigningConfigArn
      ]

instance Data.ToQuery GetCodeSigningConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCodeSigningConfigResponse' smart constructor.
data GetCodeSigningConfigResponse = GetCodeSigningConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The code signing configuration
    codeSigningConfig :: CodeSigningConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCodeSigningConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getCodeSigningConfigResponse_httpStatus' - The response's http status code.
--
-- 'codeSigningConfig', 'getCodeSigningConfigResponse_codeSigningConfig' - The code signing configuration
newGetCodeSigningConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'codeSigningConfig'
  CodeSigningConfig ->
  GetCodeSigningConfigResponse
newGetCodeSigningConfigResponse
  pHttpStatus_
  pCodeSigningConfig_ =
    GetCodeSigningConfigResponse'
      { httpStatus =
          pHttpStatus_,
        codeSigningConfig = pCodeSigningConfig_
      }

-- | The response's http status code.
getCodeSigningConfigResponse_httpStatus :: Lens.Lens' GetCodeSigningConfigResponse Prelude.Int
getCodeSigningConfigResponse_httpStatus = Lens.lens (\GetCodeSigningConfigResponse' {httpStatus} -> httpStatus) (\s@GetCodeSigningConfigResponse' {} a -> s {httpStatus = a} :: GetCodeSigningConfigResponse)

-- | The code signing configuration
getCodeSigningConfigResponse_codeSigningConfig :: Lens.Lens' GetCodeSigningConfigResponse CodeSigningConfig
getCodeSigningConfigResponse_codeSigningConfig = Lens.lens (\GetCodeSigningConfigResponse' {codeSigningConfig} -> codeSigningConfig) (\s@GetCodeSigningConfigResponse' {} a -> s {codeSigningConfig = a} :: GetCodeSigningConfigResponse)

instance Prelude.NFData GetCodeSigningConfigResponse where
  rnf GetCodeSigningConfigResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf codeSigningConfig
