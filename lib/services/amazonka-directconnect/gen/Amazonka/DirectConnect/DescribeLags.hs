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
-- Module      : Amazonka.DirectConnect.DescribeLags
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all your link aggregation groups (LAG) or the specified LAG.
module Amazonka.DirectConnect.DescribeLags
  ( -- * Creating a Request
    DescribeLags (..),
    newDescribeLags,

    -- * Request Lenses
    describeLags_lagId,

    -- * Destructuring the Response
    DescribeLagsResponse (..),
    newDescribeLagsResponse,

    -- * Response Lenses
    describeLagsResponse_lags,
    describeLagsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLags' smart constructor.
data DescribeLags = DescribeLags'
  { -- | The ID of the LAG.
    lagId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lagId', 'describeLags_lagId' - The ID of the LAG.
newDescribeLags ::
  DescribeLags
newDescribeLags =
  DescribeLags' {lagId = Prelude.Nothing}

-- | The ID of the LAG.
describeLags_lagId :: Lens.Lens' DescribeLags (Prelude.Maybe Prelude.Text)
describeLags_lagId = Lens.lens (\DescribeLags' {lagId} -> lagId) (\s@DescribeLags' {} a -> s {lagId = a} :: DescribeLags)

instance Core.AWSRequest DescribeLags where
  type AWSResponse DescribeLags = DescribeLagsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLagsResponse'
            Prelude.<$> (x Data..?> "lags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLags where
  hashWithSalt _salt DescribeLags' {..} =
    _salt `Prelude.hashWithSalt` lagId

instance Prelude.NFData DescribeLags where
  rnf DescribeLags' {..} = Prelude.rnf lagId

instance Data.ToHeaders DescribeLags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.DescribeLags" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLags where
  toJSON DescribeLags' {..} =
    Data.object
      ( Prelude.catMaybes
          [("lagId" Data..=) Prelude.<$> lagId]
      )

instance Data.ToPath DescribeLags where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLagsResponse' smart constructor.
data DescribeLagsResponse = DescribeLagsResponse'
  { -- | The LAGs.
    lags :: Prelude.Maybe [Lag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lags', 'describeLagsResponse_lags' - The LAGs.
--
-- 'httpStatus', 'describeLagsResponse_httpStatus' - The response's http status code.
newDescribeLagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLagsResponse
newDescribeLagsResponse pHttpStatus_ =
  DescribeLagsResponse'
    { lags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The LAGs.
describeLagsResponse_lags :: Lens.Lens' DescribeLagsResponse (Prelude.Maybe [Lag])
describeLagsResponse_lags = Lens.lens (\DescribeLagsResponse' {lags} -> lags) (\s@DescribeLagsResponse' {} a -> s {lags = a} :: DescribeLagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeLagsResponse_httpStatus :: Lens.Lens' DescribeLagsResponse Prelude.Int
describeLagsResponse_httpStatus = Lens.lens (\DescribeLagsResponse' {httpStatus} -> httpStatus) (\s@DescribeLagsResponse' {} a -> s {httpStatus = a} :: DescribeLagsResponse)

instance Prelude.NFData DescribeLagsResponse where
  rnf DescribeLagsResponse' {..} =
    Prelude.rnf lags
      `Prelude.seq` Prelude.rnf httpStatus
