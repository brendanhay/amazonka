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
-- Module      : Amazonka.CodeGuruReviewer.DescribeCodeReview
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the metadata associated with the code review along with its
-- status.
module Amazonka.CodeGuruReviewer.DescribeCodeReview
  ( -- * Creating a Request
    DescribeCodeReview (..),
    newDescribeCodeReview,

    -- * Request Lenses
    describeCodeReview_codeReviewArn,

    -- * Destructuring the Response
    DescribeCodeReviewResponse (..),
    newDescribeCodeReviewResponse,

    -- * Response Lenses
    describeCodeReviewResponse_codeReview,
    describeCodeReviewResponse_httpStatus,
  )
where

import Amazonka.CodeGuruReviewer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCodeReview' smart constructor.
data DescribeCodeReview = DescribeCodeReview'
  { -- | The Amazon Resource Name (ARN) of the
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
    -- object.
    codeReviewArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCodeReview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeReviewArn', 'describeCodeReview_codeReviewArn' - The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
-- object.
newDescribeCodeReview ::
  -- | 'codeReviewArn'
  Prelude.Text ->
  DescribeCodeReview
newDescribeCodeReview pCodeReviewArn_ =
  DescribeCodeReview'
    { codeReviewArn =
        pCodeReviewArn_
    }

-- | The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
-- object.
describeCodeReview_codeReviewArn :: Lens.Lens' DescribeCodeReview Prelude.Text
describeCodeReview_codeReviewArn = Lens.lens (\DescribeCodeReview' {codeReviewArn} -> codeReviewArn) (\s@DescribeCodeReview' {} a -> s {codeReviewArn = a} :: DescribeCodeReview)

instance Core.AWSRequest DescribeCodeReview where
  type
    AWSResponse DescribeCodeReview =
      DescribeCodeReviewResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCodeReviewResponse'
            Prelude.<$> (x Data..?> "CodeReview")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCodeReview where
  hashWithSalt _salt DescribeCodeReview' {..} =
    _salt `Prelude.hashWithSalt` codeReviewArn

instance Prelude.NFData DescribeCodeReview where
  rnf DescribeCodeReview' {..} =
    Prelude.rnf codeReviewArn

instance Data.ToHeaders DescribeCodeReview where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeCodeReview where
  toPath DescribeCodeReview' {..} =
    Prelude.mconcat
      ["/codereviews/", Data.toBS codeReviewArn]

instance Data.ToQuery DescribeCodeReview where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCodeReviewResponse' smart constructor.
data DescribeCodeReviewResponse = DescribeCodeReviewResponse'
  { -- | Information about the code review.
    codeReview :: Prelude.Maybe CodeReview,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCodeReviewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeReview', 'describeCodeReviewResponse_codeReview' - Information about the code review.
--
-- 'httpStatus', 'describeCodeReviewResponse_httpStatus' - The response's http status code.
newDescribeCodeReviewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCodeReviewResponse
newDescribeCodeReviewResponse pHttpStatus_ =
  DescribeCodeReviewResponse'
    { codeReview =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the code review.
describeCodeReviewResponse_codeReview :: Lens.Lens' DescribeCodeReviewResponse (Prelude.Maybe CodeReview)
describeCodeReviewResponse_codeReview = Lens.lens (\DescribeCodeReviewResponse' {codeReview} -> codeReview) (\s@DescribeCodeReviewResponse' {} a -> s {codeReview = a} :: DescribeCodeReviewResponse)

-- | The response's http status code.
describeCodeReviewResponse_httpStatus :: Lens.Lens' DescribeCodeReviewResponse Prelude.Int
describeCodeReviewResponse_httpStatus = Lens.lens (\DescribeCodeReviewResponse' {httpStatus} -> httpStatus) (\s@DescribeCodeReviewResponse' {} a -> s {httpStatus = a} :: DescribeCodeReviewResponse)

instance Prelude.NFData DescribeCodeReviewResponse where
  rnf DescribeCodeReviewResponse' {..} =
    Prelude.rnf codeReview
      `Prelude.seq` Prelude.rnf httpStatus
