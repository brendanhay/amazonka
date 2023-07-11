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
-- Module      : Amazonka.Personalize.DescribeFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a filter\'s properties.
module Amazonka.Personalize.DescribeFilter
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFilterResponse'
            Prelude.<$> (x Data..?> "filter")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFilter where
  hashWithSalt _salt DescribeFilter' {..} =
    _salt `Prelude.hashWithSalt` filterArn

instance Prelude.NFData DescribeFilter where
  rnf DescribeFilter' {..} = Prelude.rnf filterArn

instance Data.ToHeaders DescribeFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.DescribeFilter" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFilter where
  toJSON DescribeFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("filterArn" Data..= filterArn)]
      )

instance Data.ToPath DescribeFilter where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFilter where
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

instance Prelude.NFData DescribeFilterResponse where
  rnf DescribeFilterResponse' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf httpStatus
