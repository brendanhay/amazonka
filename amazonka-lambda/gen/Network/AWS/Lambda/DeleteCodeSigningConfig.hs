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
-- Module      : Network.AWS.Lambda.DeleteCodeSigningConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the code signing configuration. You can delete the code signing
-- configuration only if no function is using it.
module Network.AWS.Lambda.DeleteCodeSigningConfig
  ( -- * Creating a Request
    DeleteCodeSigningConfig (..),
    newDeleteCodeSigningConfig,

    -- * Request Lenses
    deleteCodeSigningConfig_codeSigningConfigArn,

    -- * Destructuring the Response
    DeleteCodeSigningConfigResponse (..),
    newDeleteCodeSigningConfigResponse,

    -- * Response Lenses
    deleteCodeSigningConfigResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCodeSigningConfig' smart constructor.
data DeleteCodeSigningConfig = DeleteCodeSigningConfig'
  { -- | The The Amazon Resource Name (ARN) of the code signing configuration.
    codeSigningConfigArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCodeSigningConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeSigningConfigArn', 'deleteCodeSigningConfig_codeSigningConfigArn' - The The Amazon Resource Name (ARN) of the code signing configuration.
newDeleteCodeSigningConfig ::
  -- | 'codeSigningConfigArn'
  Core.Text ->
  DeleteCodeSigningConfig
newDeleteCodeSigningConfig pCodeSigningConfigArn_ =
  DeleteCodeSigningConfig'
    { codeSigningConfigArn =
        pCodeSigningConfigArn_
    }

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
deleteCodeSigningConfig_codeSigningConfigArn :: Lens.Lens' DeleteCodeSigningConfig Core.Text
deleteCodeSigningConfig_codeSigningConfigArn = Lens.lens (\DeleteCodeSigningConfig' {codeSigningConfigArn} -> codeSigningConfigArn) (\s@DeleteCodeSigningConfig' {} a -> s {codeSigningConfigArn = a} :: DeleteCodeSigningConfig)

instance Core.AWSRequest DeleteCodeSigningConfig where
  type
    AWSResponse DeleteCodeSigningConfig =
      DeleteCodeSigningConfigResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCodeSigningConfigResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteCodeSigningConfig

instance Core.NFData DeleteCodeSigningConfig

instance Core.ToHeaders DeleteCodeSigningConfig where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteCodeSigningConfig where
  toPath DeleteCodeSigningConfig' {..} =
    Core.mconcat
      [ "/2020-04-22/code-signing-configs/",
        Core.toBS codeSigningConfigArn
      ]

instance Core.ToQuery DeleteCodeSigningConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteCodeSigningConfigResponse' smart constructor.
data DeleteCodeSigningConfigResponse = DeleteCodeSigningConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCodeSigningConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCodeSigningConfigResponse_httpStatus' - The response's http status code.
newDeleteCodeSigningConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteCodeSigningConfigResponse
newDeleteCodeSigningConfigResponse pHttpStatus_ =
  DeleteCodeSigningConfigResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteCodeSigningConfigResponse_httpStatus :: Lens.Lens' DeleteCodeSigningConfigResponse Core.Int
deleteCodeSigningConfigResponse_httpStatus = Lens.lens (\DeleteCodeSigningConfigResponse' {httpStatus} -> httpStatus) (\s@DeleteCodeSigningConfigResponse' {} a -> s {httpStatus = a} :: DeleteCodeSigningConfigResponse)

instance Core.NFData DeleteCodeSigningConfigResponse
