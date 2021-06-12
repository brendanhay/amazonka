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
-- Module      : Network.AWS.ServiceCatalog.DescribeTagOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified TagOption.
module Network.AWS.ServiceCatalog.DescribeTagOption
  ( -- * Creating a Request
    DescribeTagOption (..),
    newDescribeTagOption,

    -- * Request Lenses
    describeTagOption_id,

    -- * Destructuring the Response
    DescribeTagOptionResponse (..),
    newDescribeTagOptionResponse,

    -- * Response Lenses
    describeTagOptionResponse_tagOptionDetail,
    describeTagOptionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDescribeTagOption' smart constructor.
data DescribeTagOption = DescribeTagOption'
  { -- | The TagOption identifier.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTagOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeTagOption_id' - The TagOption identifier.
newDescribeTagOption ::
  -- | 'id'
  Core.Text ->
  DescribeTagOption
newDescribeTagOption pId_ =
  DescribeTagOption' {id = pId_}

-- | The TagOption identifier.
describeTagOption_id :: Lens.Lens' DescribeTagOption Core.Text
describeTagOption_id = Lens.lens (\DescribeTagOption' {id} -> id) (\s@DescribeTagOption' {} a -> s {id = a} :: DescribeTagOption)

instance Core.AWSRequest DescribeTagOption where
  type
    AWSResponse DescribeTagOption =
      DescribeTagOptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTagOptionResponse'
            Core.<$> (x Core..?> "TagOptionDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTagOption

instance Core.NFData DescribeTagOption

instance Core.ToHeaders DescribeTagOption where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribeTagOption" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTagOption where
  toJSON DescribeTagOption' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Id" Core..= id)])

instance Core.ToPath DescribeTagOption where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTagOption where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeTagOptionResponse' smart constructor.
data DescribeTagOptionResponse = DescribeTagOptionResponse'
  { -- | Information about the TagOption.
    tagOptionDetail :: Core.Maybe TagOptionDetail,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTagOptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagOptionDetail', 'describeTagOptionResponse_tagOptionDetail' - Information about the TagOption.
--
-- 'httpStatus', 'describeTagOptionResponse_httpStatus' - The response's http status code.
newDescribeTagOptionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTagOptionResponse
newDescribeTagOptionResponse pHttpStatus_ =
  DescribeTagOptionResponse'
    { tagOptionDetail =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the TagOption.
describeTagOptionResponse_tagOptionDetail :: Lens.Lens' DescribeTagOptionResponse (Core.Maybe TagOptionDetail)
describeTagOptionResponse_tagOptionDetail = Lens.lens (\DescribeTagOptionResponse' {tagOptionDetail} -> tagOptionDetail) (\s@DescribeTagOptionResponse' {} a -> s {tagOptionDetail = a} :: DescribeTagOptionResponse)

-- | The response's http status code.
describeTagOptionResponse_httpStatus :: Lens.Lens' DescribeTagOptionResponse Core.Int
describeTagOptionResponse_httpStatus = Lens.lens (\DescribeTagOptionResponse' {httpStatus} -> httpStatus) (\s@DescribeTagOptionResponse' {} a -> s {httpStatus = a} :: DescribeTagOptionResponse)

instance Core.NFData DescribeTagOptionResponse
