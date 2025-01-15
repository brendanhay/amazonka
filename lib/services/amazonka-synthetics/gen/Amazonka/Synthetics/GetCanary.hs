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
-- Module      : Amazonka.Synthetics.GetCanary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves complete information about one canary. You must specify the
-- name of the canary that you want. To get a list of canaries and their
-- names, use
-- <https://docs.aws.amazon.com/AmazonSynthetics/latest/APIReference/API_DescribeCanaries.html DescribeCanaries>.
module Amazonka.Synthetics.GetCanary
  ( -- * Creating a Request
    GetCanary (..),
    newGetCanary,

    -- * Request Lenses
    getCanary_name,

    -- * Destructuring the Response
    GetCanaryResponse (..),
    newGetCanaryResponse,

    -- * Response Lenses
    getCanaryResponse_canary,
    getCanaryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Synthetics.Types

-- | /See:/ 'newGetCanary' smart constructor.
data GetCanary = GetCanary'
  { -- | The name of the canary that you want details for.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCanary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getCanary_name' - The name of the canary that you want details for.
newGetCanary ::
  -- | 'name'
  Prelude.Text ->
  GetCanary
newGetCanary pName_ = GetCanary' {name = pName_}

-- | The name of the canary that you want details for.
getCanary_name :: Lens.Lens' GetCanary Prelude.Text
getCanary_name = Lens.lens (\GetCanary' {name} -> name) (\s@GetCanary' {} a -> s {name = a} :: GetCanary)

instance Core.AWSRequest GetCanary where
  type AWSResponse GetCanary = GetCanaryResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCanaryResponse'
            Prelude.<$> (x Data..?> "Canary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCanary where
  hashWithSalt _salt GetCanary' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetCanary where
  rnf GetCanary' {..} = Prelude.rnf name

instance Data.ToHeaders GetCanary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCanary where
  toPath GetCanary' {..} =
    Prelude.mconcat ["/canary/", Data.toBS name]

instance Data.ToQuery GetCanary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCanaryResponse' smart constructor.
data GetCanaryResponse = GetCanaryResponse'
  { -- | A structure that contains the full information about the canary.
    canary :: Prelude.Maybe Canary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCanaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'canary', 'getCanaryResponse_canary' - A structure that contains the full information about the canary.
--
-- 'httpStatus', 'getCanaryResponse_httpStatus' - The response's http status code.
newGetCanaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCanaryResponse
newGetCanaryResponse pHttpStatus_ =
  GetCanaryResponse'
    { canary = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains the full information about the canary.
getCanaryResponse_canary :: Lens.Lens' GetCanaryResponse (Prelude.Maybe Canary)
getCanaryResponse_canary = Lens.lens (\GetCanaryResponse' {canary} -> canary) (\s@GetCanaryResponse' {} a -> s {canary = a} :: GetCanaryResponse)

-- | The response's http status code.
getCanaryResponse_httpStatus :: Lens.Lens' GetCanaryResponse Prelude.Int
getCanaryResponse_httpStatus = Lens.lens (\GetCanaryResponse' {httpStatus} -> httpStatus) (\s@GetCanaryResponse' {} a -> s {httpStatus = a} :: GetCanaryResponse)

instance Prelude.NFData GetCanaryResponse where
  rnf GetCanaryResponse' {..} =
    Prelude.rnf canary `Prelude.seq`
      Prelude.rnf httpStatus
