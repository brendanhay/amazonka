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
-- Module      : Amazonka.DirectConnect.DescribeInterconnects
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the interconnects owned by the Amazon Web Services account or only
-- the specified interconnect.
module Amazonka.DirectConnect.DescribeInterconnects
  ( -- * Creating a Request
    DescribeInterconnects (..),
    newDescribeInterconnects,

    -- * Request Lenses
    describeInterconnects_interconnectId,

    -- * Destructuring the Response
    DescribeInterconnectsResponse (..),
    newDescribeInterconnectsResponse,

    -- * Response Lenses
    describeInterconnectsResponse_interconnects,
    describeInterconnectsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeInterconnects' smart constructor.
data DescribeInterconnects = DescribeInterconnects'
  { -- | The ID of the interconnect.
    interconnectId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInterconnects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'interconnectId', 'describeInterconnects_interconnectId' - The ID of the interconnect.
newDescribeInterconnects ::
  DescribeInterconnects
newDescribeInterconnects =
  DescribeInterconnects'
    { interconnectId =
        Prelude.Nothing
    }

-- | The ID of the interconnect.
describeInterconnects_interconnectId :: Lens.Lens' DescribeInterconnects (Prelude.Maybe Prelude.Text)
describeInterconnects_interconnectId = Lens.lens (\DescribeInterconnects' {interconnectId} -> interconnectId) (\s@DescribeInterconnects' {} a -> s {interconnectId = a} :: DescribeInterconnects)

instance Core.AWSRequest DescribeInterconnects where
  type
    AWSResponse DescribeInterconnects =
      DescribeInterconnectsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInterconnectsResponse'
            Prelude.<$> (x Core..?> "interconnects" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInterconnects where
  hashWithSalt _salt DescribeInterconnects' {..} =
    _salt `Prelude.hashWithSalt` interconnectId

instance Prelude.NFData DescribeInterconnects where
  rnf DescribeInterconnects' {..} =
    Prelude.rnf interconnectId

instance Core.ToHeaders DescribeInterconnects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.DescribeInterconnects" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeInterconnects where
  toJSON DescribeInterconnects' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("interconnectId" Core..=)
              Prelude.<$> interconnectId
          ]
      )

instance Core.ToPath DescribeInterconnects where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeInterconnects where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeInterconnectsResponse' smart constructor.
data DescribeInterconnectsResponse = DescribeInterconnectsResponse'
  { -- | The interconnects.
    interconnects :: Prelude.Maybe [Interconnect],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInterconnectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'interconnects', 'describeInterconnectsResponse_interconnects' - The interconnects.
--
-- 'httpStatus', 'describeInterconnectsResponse_httpStatus' - The response's http status code.
newDescribeInterconnectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInterconnectsResponse
newDescribeInterconnectsResponse pHttpStatus_ =
  DescribeInterconnectsResponse'
    { interconnects =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The interconnects.
describeInterconnectsResponse_interconnects :: Lens.Lens' DescribeInterconnectsResponse (Prelude.Maybe [Interconnect])
describeInterconnectsResponse_interconnects = Lens.lens (\DescribeInterconnectsResponse' {interconnects} -> interconnects) (\s@DescribeInterconnectsResponse' {} a -> s {interconnects = a} :: DescribeInterconnectsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeInterconnectsResponse_httpStatus :: Lens.Lens' DescribeInterconnectsResponse Prelude.Int
describeInterconnectsResponse_httpStatus = Lens.lens (\DescribeInterconnectsResponse' {httpStatus} -> httpStatus) (\s@DescribeInterconnectsResponse' {} a -> s {httpStatus = a} :: DescribeInterconnectsResponse)

instance Prelude.NFData DescribeInterconnectsResponse where
  rnf DescribeInterconnectsResponse' {..} =
    Prelude.rnf interconnects
      `Prelude.seq` Prelude.rnf httpStatus
