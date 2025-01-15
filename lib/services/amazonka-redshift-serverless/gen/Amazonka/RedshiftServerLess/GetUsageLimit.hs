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
-- Module      : Amazonka.RedshiftServerLess.GetUsageLimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a usage limit.
module Amazonka.RedshiftServerLess.GetUsageLimit
  ( -- * Creating a Request
    GetUsageLimit (..),
    newGetUsageLimit,

    -- * Request Lenses
    getUsageLimit_usageLimitId,

    -- * Destructuring the Response
    GetUsageLimitResponse (..),
    newGetUsageLimitResponse,

    -- * Response Lenses
    getUsageLimitResponse_usageLimit,
    getUsageLimitResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUsageLimit' smart constructor.
data GetUsageLimit = GetUsageLimit'
  { -- | The unique identifier of the usage limit to return information for.
    usageLimitId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUsageLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usageLimitId', 'getUsageLimit_usageLimitId' - The unique identifier of the usage limit to return information for.
newGetUsageLimit ::
  -- | 'usageLimitId'
  Prelude.Text ->
  GetUsageLimit
newGetUsageLimit pUsageLimitId_ =
  GetUsageLimit' {usageLimitId = pUsageLimitId_}

-- | The unique identifier of the usage limit to return information for.
getUsageLimit_usageLimitId :: Lens.Lens' GetUsageLimit Prelude.Text
getUsageLimit_usageLimitId = Lens.lens (\GetUsageLimit' {usageLimitId} -> usageLimitId) (\s@GetUsageLimit' {} a -> s {usageLimitId = a} :: GetUsageLimit)

instance Core.AWSRequest GetUsageLimit where
  type
    AWSResponse GetUsageLimit =
      GetUsageLimitResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUsageLimitResponse'
            Prelude.<$> (x Data..?> "usageLimit")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetUsageLimit where
  hashWithSalt _salt GetUsageLimit' {..} =
    _salt `Prelude.hashWithSalt` usageLimitId

instance Prelude.NFData GetUsageLimit where
  rnf GetUsageLimit' {..} = Prelude.rnf usageLimitId

instance Data.ToHeaders GetUsageLimit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.GetUsageLimit" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetUsageLimit where
  toJSON GetUsageLimit' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("usageLimitId" Data..= usageLimitId)]
      )

instance Data.ToPath GetUsageLimit where
  toPath = Prelude.const "/"

instance Data.ToQuery GetUsageLimit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetUsageLimitResponse' smart constructor.
data GetUsageLimitResponse = GetUsageLimitResponse'
  { -- | The returned usage limit object.
    usageLimit :: Prelude.Maybe UsageLimit,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUsageLimitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usageLimit', 'getUsageLimitResponse_usageLimit' - The returned usage limit object.
--
-- 'httpStatus', 'getUsageLimitResponse_httpStatus' - The response's http status code.
newGetUsageLimitResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetUsageLimitResponse
newGetUsageLimitResponse pHttpStatus_ =
  GetUsageLimitResponse'
    { usageLimit =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The returned usage limit object.
getUsageLimitResponse_usageLimit :: Lens.Lens' GetUsageLimitResponse (Prelude.Maybe UsageLimit)
getUsageLimitResponse_usageLimit = Lens.lens (\GetUsageLimitResponse' {usageLimit} -> usageLimit) (\s@GetUsageLimitResponse' {} a -> s {usageLimit = a} :: GetUsageLimitResponse)

-- | The response's http status code.
getUsageLimitResponse_httpStatus :: Lens.Lens' GetUsageLimitResponse Prelude.Int
getUsageLimitResponse_httpStatus = Lens.lens (\GetUsageLimitResponse' {httpStatus} -> httpStatus) (\s@GetUsageLimitResponse' {} a -> s {httpStatus = a} :: GetUsageLimitResponse)

instance Prelude.NFData GetUsageLimitResponse where
  rnf GetUsageLimitResponse' {..} =
    Prelude.rnf usageLimit `Prelude.seq`
      Prelude.rnf httpStatus
