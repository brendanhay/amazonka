{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.ClearDefaultAuthorizer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Clears the default authorizer.
module Network.AWS.IoT.ClearDefaultAuthorizer
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newClearDefaultAuthorizer' smart constructor.
data ClearDefaultAuthorizer = ClearDefaultAuthorizer'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ClearDefaultAuthorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newClearDefaultAuthorizer ::
  ClearDefaultAuthorizer
newClearDefaultAuthorizer = ClearDefaultAuthorizer'

instance Prelude.AWSRequest ClearDefaultAuthorizer where
  type
    Rs ClearDefaultAuthorizer =
      ClearDefaultAuthorizerResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ClearDefaultAuthorizerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ClearDefaultAuthorizer

instance Prelude.NFData ClearDefaultAuthorizer

instance Prelude.ToHeaders ClearDefaultAuthorizer where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ClearDefaultAuthorizer where
  toPath = Prelude.const "/default-authorizer"

instance Prelude.ToQuery ClearDefaultAuthorizer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newClearDefaultAuthorizerResponse' smart constructor.
data ClearDefaultAuthorizerResponse = ClearDefaultAuthorizerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
