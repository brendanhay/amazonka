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
-- Module      : Amazonka.ManagedBlockChain.GetAccessor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The token based access feature is in preview release for Ethereum on
-- Amazon Managed Blockchain and is subject to change. We recommend that
-- you use this feature only with test scenarios, and not in production
-- environments.
--
-- Returns detailed information about an accessor. An accessor object is a
-- container that has the information required for token based access to
-- your Ethereum nodes.
module Amazonka.ManagedBlockChain.GetAccessor
  ( -- * Creating a Request
    GetAccessor (..),
    newGetAccessor,

    -- * Request Lenses
    getAccessor_accessorId,

    -- * Destructuring the Response
    GetAccessorResponse (..),
    newGetAccessorResponse,

    -- * Response Lenses
    getAccessorResponse_accessor,
    getAccessorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAccessor' smart constructor.
data GetAccessor = GetAccessor'
  { -- | The unique identifier of the accessor.
    accessorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccessor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessorId', 'getAccessor_accessorId' - The unique identifier of the accessor.
newGetAccessor ::
  -- | 'accessorId'
  Prelude.Text ->
  GetAccessor
newGetAccessor pAccessorId_ =
  GetAccessor' {accessorId = pAccessorId_}

-- | The unique identifier of the accessor.
getAccessor_accessorId :: Lens.Lens' GetAccessor Prelude.Text
getAccessor_accessorId = Lens.lens (\GetAccessor' {accessorId} -> accessorId) (\s@GetAccessor' {} a -> s {accessorId = a} :: GetAccessor)

instance Core.AWSRequest GetAccessor where
  type AWSResponse GetAccessor = GetAccessorResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccessorResponse'
            Prelude.<$> (x Data..?> "Accessor")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccessor where
  hashWithSalt _salt GetAccessor' {..} =
    _salt `Prelude.hashWithSalt` accessorId

instance Prelude.NFData GetAccessor where
  rnf GetAccessor' {..} = Prelude.rnf accessorId

instance Data.ToHeaders GetAccessor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAccessor where
  toPath GetAccessor' {..} =
    Prelude.mconcat
      ["/accessors/", Data.toBS accessorId]

instance Data.ToQuery GetAccessor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAccessorResponse' smart constructor.
data GetAccessorResponse = GetAccessorResponse'
  { -- | The properties of the accessor.
    accessor :: Prelude.Maybe Accessor,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccessorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessor', 'getAccessorResponse_accessor' - The properties of the accessor.
--
-- 'httpStatus', 'getAccessorResponse_httpStatus' - The response's http status code.
newGetAccessorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAccessorResponse
newGetAccessorResponse pHttpStatus_ =
  GetAccessorResponse'
    { accessor = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The properties of the accessor.
getAccessorResponse_accessor :: Lens.Lens' GetAccessorResponse (Prelude.Maybe Accessor)
getAccessorResponse_accessor = Lens.lens (\GetAccessorResponse' {accessor} -> accessor) (\s@GetAccessorResponse' {} a -> s {accessor = a} :: GetAccessorResponse)

-- | The response's http status code.
getAccessorResponse_httpStatus :: Lens.Lens' GetAccessorResponse Prelude.Int
getAccessorResponse_httpStatus = Lens.lens (\GetAccessorResponse' {httpStatus} -> httpStatus) (\s@GetAccessorResponse' {} a -> s {httpStatus = a} :: GetAccessorResponse)

instance Prelude.NFData GetAccessorResponse where
  rnf GetAccessorResponse' {..} =
    Prelude.rnf accessor
      `Prelude.seq` Prelude.rnf httpStatus
