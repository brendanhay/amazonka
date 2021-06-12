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
-- Module      : Network.AWS.DirectConnect.DescribeInterconnects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the interconnects owned by the AWS account or only the specified
-- interconnect.
module Network.AWS.DirectConnect.DescribeInterconnects
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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeInterconnects' smart constructor.
data DescribeInterconnects = DescribeInterconnects'
  { -- | The ID of the interconnect.
    interconnectId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | The ID of the interconnect.
describeInterconnects_interconnectId :: Lens.Lens' DescribeInterconnects (Core.Maybe Core.Text)
describeInterconnects_interconnectId = Lens.lens (\DescribeInterconnects' {interconnectId} -> interconnectId) (\s@DescribeInterconnects' {} a -> s {interconnectId = a} :: DescribeInterconnects)

instance Core.AWSRequest DescribeInterconnects where
  type
    AWSResponse DescribeInterconnects =
      DescribeInterconnectsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInterconnectsResponse'
            Core.<$> (x Core..?> "interconnects" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeInterconnects

instance Core.NFData DescribeInterconnects

instance Core.ToHeaders DescribeInterconnects where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.DescribeInterconnects" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeInterconnects where
  toJSON DescribeInterconnects' {..} =
    Core.object
      ( Core.catMaybes
          [("interconnectId" Core..=) Core.<$> interconnectId]
      )

instance Core.ToPath DescribeInterconnects where
  toPath = Core.const "/"

instance Core.ToQuery DescribeInterconnects where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeInterconnectsResponse' smart constructor.
data DescribeInterconnectsResponse = DescribeInterconnectsResponse'
  { -- | The interconnects.
    interconnects :: Core.Maybe [Interconnect],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeInterconnectsResponse
newDescribeInterconnectsResponse pHttpStatus_ =
  DescribeInterconnectsResponse'
    { interconnects =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The interconnects.
describeInterconnectsResponse_interconnects :: Lens.Lens' DescribeInterconnectsResponse (Core.Maybe [Interconnect])
describeInterconnectsResponse_interconnects = Lens.lens (\DescribeInterconnectsResponse' {interconnects} -> interconnects) (\s@DescribeInterconnectsResponse' {} a -> s {interconnects = a} :: DescribeInterconnectsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeInterconnectsResponse_httpStatus :: Lens.Lens' DescribeInterconnectsResponse Core.Int
describeInterconnectsResponse_httpStatus = Lens.lens (\DescribeInterconnectsResponse' {httpStatus} -> httpStatus) (\s@DescribeInterconnectsResponse' {} a -> s {httpStatus = a} :: DescribeInterconnectsResponse)

instance Core.NFData DescribeInterconnectsResponse
