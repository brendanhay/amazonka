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
-- Module      : Amazonka.Lambda.DeleteCodeSigningConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the code signing configuration. You can delete the code signing
-- configuration only if no function is using it.
module Amazonka.Lambda.DeleteCodeSigningConfig
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCodeSigningConfig' smart constructor.
data DeleteCodeSigningConfig = DeleteCodeSigningConfig'
  { -- | The The Amazon Resource Name (ARN) of the code signing configuration.
    codeSigningConfigArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteCodeSigningConfig
newDeleteCodeSigningConfig pCodeSigningConfigArn_ =
  DeleteCodeSigningConfig'
    { codeSigningConfigArn =
        pCodeSigningConfigArn_
    }

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
deleteCodeSigningConfig_codeSigningConfigArn :: Lens.Lens' DeleteCodeSigningConfig Prelude.Text
deleteCodeSigningConfig_codeSigningConfigArn = Lens.lens (\DeleteCodeSigningConfig' {codeSigningConfigArn} -> codeSigningConfigArn) (\s@DeleteCodeSigningConfig' {} a -> s {codeSigningConfigArn = a} :: DeleteCodeSigningConfig)

instance Core.AWSRequest DeleteCodeSigningConfig where
  type
    AWSResponse DeleteCodeSigningConfig =
      DeleteCodeSigningConfigResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCodeSigningConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCodeSigningConfig where
  hashWithSalt _salt DeleteCodeSigningConfig' {..} =
    _salt `Prelude.hashWithSalt` codeSigningConfigArn

instance Prelude.NFData DeleteCodeSigningConfig where
  rnf DeleteCodeSigningConfig' {..} =
    Prelude.rnf codeSigningConfigArn

instance Core.ToHeaders DeleteCodeSigningConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteCodeSigningConfig where
  toPath DeleteCodeSigningConfig' {..} =
    Prelude.mconcat
      [ "/2020-04-22/code-signing-configs/",
        Core.toBS codeSigningConfigArn
      ]

instance Core.ToQuery DeleteCodeSigningConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCodeSigningConfigResponse' smart constructor.
data DeleteCodeSigningConfigResponse = DeleteCodeSigningConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteCodeSigningConfigResponse
newDeleteCodeSigningConfigResponse pHttpStatus_ =
  DeleteCodeSigningConfigResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteCodeSigningConfigResponse_httpStatus :: Lens.Lens' DeleteCodeSigningConfigResponse Prelude.Int
deleteCodeSigningConfigResponse_httpStatus = Lens.lens (\DeleteCodeSigningConfigResponse' {httpStatus} -> httpStatus) (\s@DeleteCodeSigningConfigResponse' {} a -> s {httpStatus = a} :: DeleteCodeSigningConfigResponse)

instance
  Prelude.NFData
    DeleteCodeSigningConfigResponse
  where
  rnf DeleteCodeSigningConfigResponse' {..} =
    Prelude.rnf httpStatus
