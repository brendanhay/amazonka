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
-- Module      : Network.AWS.Personalize.DescribeFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a filter\'s properties.
module Network.AWS.Personalize.DescribeFilter
  ( -- * Creating a Request
    DescribeFilter (..),
    newDescribeFilter,

    -- * Request Lenses
    describeFilter_filterArn,

    -- * Destructuring the Response
    DescribeFilterResponse (..),
    newDescribeFilterResponse,

    -- * Response Lenses
    describeFilterResponse_filter,
    describeFilterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Personalize.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeFilter' smart constructor.
data DescribeFilter = DescribeFilter'
  { -- | The ARN of the filter to describe.
    filterArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterArn', 'describeFilter_filterArn' - The ARN of the filter to describe.
newDescribeFilter ::
  -- | 'filterArn'
  Prelude.Text ->
  DescribeFilter
newDescribeFilter pFilterArn_ =
  DescribeFilter' {filterArn = pFilterArn_}

-- | The ARN of the filter to describe.
describeFilter_filterArn :: Lens.Lens' DescribeFilter Prelude.Text
describeFilter_filterArn = Lens.lens (\DescribeFilter' {filterArn} -> filterArn) (\s@DescribeFilter' {} a -> s {filterArn = a} :: DescribeFilter)

instance Core.AWSRequest DescribeFilter where
  type
    AWSResponse DescribeFilter =
      DescribeFilterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFilterResponse'
            Prelude.<$> (x Core..?> "filter")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFilter

instance Prelude.NFData DescribeFilter

instance Core.ToHeaders DescribeFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonPersonalize.DescribeFilter" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeFilter where
  toJSON DescribeFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("filterArn" Core..= filterArn)]
      )

instance Core.ToPath DescribeFilter where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFilterResponse' smart constructor.
data DescribeFilterResponse = DescribeFilterResponse'
  { -- | The filter\'s details.
    filter' :: Prelude.Maybe Filter,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'describeFilterResponse_filter' - The filter\'s details.
--
-- 'httpStatus', 'describeFilterResponse_httpStatus' - The response's http status code.
newDescribeFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFilterResponse
newDescribeFilterResponse pHttpStatus_ =
  DescribeFilterResponse'
    { filter' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The filter\'s details.
describeFilterResponse_filter :: Lens.Lens' DescribeFilterResponse (Prelude.Maybe Filter)
describeFilterResponse_filter = Lens.lens (\DescribeFilterResponse' {filter'} -> filter') (\s@DescribeFilterResponse' {} a -> s {filter' = a} :: DescribeFilterResponse)

-- | The response's http status code.
describeFilterResponse_httpStatus :: Lens.Lens' DescribeFilterResponse Prelude.Int
describeFilterResponse_httpStatus = Lens.lens (\DescribeFilterResponse' {httpStatus} -> httpStatus) (\s@DescribeFilterResponse' {} a -> s {httpStatus = a} :: DescribeFilterResponse)

instance Prelude.NFData DescribeFilterResponse
