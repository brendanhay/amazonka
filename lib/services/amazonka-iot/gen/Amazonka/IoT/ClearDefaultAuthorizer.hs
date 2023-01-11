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
-- Module      : Amazonka.IoT.ClearDefaultAuthorizer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Clears the default authorizer.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ClearDefaultAuthorizer>
-- action.
module Amazonka.IoT.ClearDefaultAuthorizer
  ( -- * Creating a Request
    ClearDefaultAuthorizer (..),
    newClearDefaultAuthorizer,

    -- * Destructuring the Response
    ClearDefaultAuthorizerResponse (..),
    newClearDefaultAuthorizerResponse,

    -- * Response Lenses
    clearDefaultAuthorizerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newClearDefaultAuthorizer' smart constructor.
data ClearDefaultAuthorizer = ClearDefaultAuthorizer'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClearDefaultAuthorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newClearDefaultAuthorizer ::
  ClearDefaultAuthorizer
newClearDefaultAuthorizer = ClearDefaultAuthorizer'

instance Core.AWSRequest ClearDefaultAuthorizer where
  type
    AWSResponse ClearDefaultAuthorizer =
      ClearDefaultAuthorizerResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ClearDefaultAuthorizerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ClearDefaultAuthorizer where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData ClearDefaultAuthorizer where
  rnf _ = ()

instance Data.ToHeaders ClearDefaultAuthorizer where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ClearDefaultAuthorizer where
  toPath = Prelude.const "/default-authorizer"

instance Data.ToQuery ClearDefaultAuthorizer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newClearDefaultAuthorizerResponse' smart constructor.
data ClearDefaultAuthorizerResponse = ClearDefaultAuthorizerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClearDefaultAuthorizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'clearDefaultAuthorizerResponse_httpStatus' - The response's http status code.
newClearDefaultAuthorizerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ClearDefaultAuthorizerResponse
newClearDefaultAuthorizerResponse pHttpStatus_ =
  ClearDefaultAuthorizerResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
clearDefaultAuthorizerResponse_httpStatus :: Lens.Lens' ClearDefaultAuthorizerResponse Prelude.Int
clearDefaultAuthorizerResponse_httpStatus = Lens.lens (\ClearDefaultAuthorizerResponse' {httpStatus} -> httpStatus) (\s@ClearDefaultAuthorizerResponse' {} a -> s {httpStatus = a} :: ClearDefaultAuthorizerResponse)

instance
  Prelude.NFData
    ClearDefaultAuthorizerResponse
  where
  rnf ClearDefaultAuthorizerResponse' {..} =
    Prelude.rnf httpStatus
