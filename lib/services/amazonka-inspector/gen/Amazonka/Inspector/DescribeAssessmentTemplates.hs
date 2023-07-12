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
-- Module      : Amazonka.Inspector.DescribeAssessmentTemplates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the assessment templates that are specified by the ARNs of the
-- assessment templates.
module Amazonka.Inspector.DescribeAssessmentTemplates
  ( -- * Creating a Request
    DescribeAssessmentTemplates (..),
    newDescribeAssessmentTemplates,

    -- * Request Lenses
    describeAssessmentTemplates_assessmentTemplateArns,

    -- * Destructuring the Response
    DescribeAssessmentTemplatesResponse (..),
    newDescribeAssessmentTemplatesResponse,

    -- * Response Lenses
    describeAssessmentTemplatesResponse_httpStatus,
    describeAssessmentTemplatesResponse_assessmentTemplates,
    describeAssessmentTemplatesResponse_failedItems,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAssessmentTemplates' smart constructor.
data DescribeAssessmentTemplates = DescribeAssessmentTemplates'
  { assessmentTemplateArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAssessmentTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentTemplateArns', 'describeAssessmentTemplates_assessmentTemplateArns' - Undocumented member.
newDescribeAssessmentTemplates ::
  -- | 'assessmentTemplateArns'
  Prelude.NonEmpty Prelude.Text ->
  DescribeAssessmentTemplates
newDescribeAssessmentTemplates
  pAssessmentTemplateArns_ =
    DescribeAssessmentTemplates'
      { assessmentTemplateArns =
          Lens.coerced Lens.# pAssessmentTemplateArns_
      }

-- | Undocumented member.
describeAssessmentTemplates_assessmentTemplateArns :: Lens.Lens' DescribeAssessmentTemplates (Prelude.NonEmpty Prelude.Text)
describeAssessmentTemplates_assessmentTemplateArns = Lens.lens (\DescribeAssessmentTemplates' {assessmentTemplateArns} -> assessmentTemplateArns) (\s@DescribeAssessmentTemplates' {} a -> s {assessmentTemplateArns = a} :: DescribeAssessmentTemplates) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeAssessmentTemplates where
  type
    AWSResponse DescribeAssessmentTemplates =
      DescribeAssessmentTemplatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssessmentTemplatesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "assessmentTemplates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "failedItems" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable DescribeAssessmentTemplates where
  hashWithSalt _salt DescribeAssessmentTemplates' {..} =
    _salt `Prelude.hashWithSalt` assessmentTemplateArns

instance Prelude.NFData DescribeAssessmentTemplates where
  rnf DescribeAssessmentTemplates' {..} =
    Prelude.rnf assessmentTemplateArns

instance Data.ToHeaders DescribeAssessmentTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.DescribeAssessmentTemplates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAssessmentTemplates where
  toJSON DescribeAssessmentTemplates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "assessmentTemplateArns"
                  Data..= assessmentTemplateArns
              )
          ]
      )

instance Data.ToPath DescribeAssessmentTemplates where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAssessmentTemplates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAssessmentTemplatesResponse' smart constructor.
data DescribeAssessmentTemplatesResponse = DescribeAssessmentTemplatesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the assessment templates.
    assessmentTemplates :: [AssessmentTemplate],
    -- | Assessment template details that cannot be described. An error code is
    -- provided for each failed item.
    failedItems :: Prelude.HashMap Prelude.Text FailedItemDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAssessmentTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeAssessmentTemplatesResponse_httpStatus' - The response's http status code.
--
-- 'assessmentTemplates', 'describeAssessmentTemplatesResponse_assessmentTemplates' - Information about the assessment templates.
--
-- 'failedItems', 'describeAssessmentTemplatesResponse_failedItems' - Assessment template details that cannot be described. An error code is
-- provided for each failed item.
newDescribeAssessmentTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAssessmentTemplatesResponse
newDescribeAssessmentTemplatesResponse pHttpStatus_ =
  DescribeAssessmentTemplatesResponse'
    { httpStatus =
        pHttpStatus_,
      assessmentTemplates = Prelude.mempty,
      failedItems = Prelude.mempty
    }

-- | The response's http status code.
describeAssessmentTemplatesResponse_httpStatus :: Lens.Lens' DescribeAssessmentTemplatesResponse Prelude.Int
describeAssessmentTemplatesResponse_httpStatus = Lens.lens (\DescribeAssessmentTemplatesResponse' {httpStatus} -> httpStatus) (\s@DescribeAssessmentTemplatesResponse' {} a -> s {httpStatus = a} :: DescribeAssessmentTemplatesResponse)

-- | Information about the assessment templates.
describeAssessmentTemplatesResponse_assessmentTemplates :: Lens.Lens' DescribeAssessmentTemplatesResponse [AssessmentTemplate]
describeAssessmentTemplatesResponse_assessmentTemplates = Lens.lens (\DescribeAssessmentTemplatesResponse' {assessmentTemplates} -> assessmentTemplates) (\s@DescribeAssessmentTemplatesResponse' {} a -> s {assessmentTemplates = a} :: DescribeAssessmentTemplatesResponse) Prelude.. Lens.coerced

-- | Assessment template details that cannot be described. An error code is
-- provided for each failed item.
describeAssessmentTemplatesResponse_failedItems :: Lens.Lens' DescribeAssessmentTemplatesResponse (Prelude.HashMap Prelude.Text FailedItemDetails)
describeAssessmentTemplatesResponse_failedItems = Lens.lens (\DescribeAssessmentTemplatesResponse' {failedItems} -> failedItems) (\s@DescribeAssessmentTemplatesResponse' {} a -> s {failedItems = a} :: DescribeAssessmentTemplatesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeAssessmentTemplatesResponse
  where
  rnf DescribeAssessmentTemplatesResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assessmentTemplates
      `Prelude.seq` Prelude.rnf failedItems
