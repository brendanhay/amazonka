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
-- Module      : Network.AWS.Inspector.DescribeAssessmentTemplates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the assessment templates that are specified by the ARNs of the
-- assessment templates.
module Network.AWS.Inspector.DescribeAssessmentTemplates
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

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
          Lens._Coerce Lens.# pAssessmentTemplateArns_
      }

-- | Undocumented member.
describeAssessmentTemplates_assessmentTemplateArns :: Lens.Lens' DescribeAssessmentTemplates (Prelude.NonEmpty Prelude.Text)
describeAssessmentTemplates_assessmentTemplateArns = Lens.lens (\DescribeAssessmentTemplates' {assessmentTemplateArns} -> assessmentTemplateArns) (\s@DescribeAssessmentTemplates' {} a -> s {assessmentTemplateArns = a} :: DescribeAssessmentTemplates) Prelude.. Lens._Coerce

instance Core.AWSRequest DescribeAssessmentTemplates where
  type
    AWSResponse DescribeAssessmentTemplates =
      DescribeAssessmentTemplatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssessmentTemplatesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "assessmentTemplates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "failedItems" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable DescribeAssessmentTemplates

instance Prelude.NFData DescribeAssessmentTemplates

instance Core.ToHeaders DescribeAssessmentTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.DescribeAssessmentTemplates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAssessmentTemplates where
  toJSON DescribeAssessmentTemplates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "assessmentTemplateArns"
                  Core..= assessmentTemplateArns
              )
          ]
      )

instance Core.ToPath DescribeAssessmentTemplates where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAssessmentTemplates where
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
describeAssessmentTemplatesResponse_assessmentTemplates = Lens.lens (\DescribeAssessmentTemplatesResponse' {assessmentTemplates} -> assessmentTemplates) (\s@DescribeAssessmentTemplatesResponse' {} a -> s {assessmentTemplates = a} :: DescribeAssessmentTemplatesResponse) Prelude.. Lens._Coerce

-- | Assessment template details that cannot be described. An error code is
-- provided for each failed item.
describeAssessmentTemplatesResponse_failedItems :: Lens.Lens' DescribeAssessmentTemplatesResponse (Prelude.HashMap Prelude.Text FailedItemDetails)
describeAssessmentTemplatesResponse_failedItems = Lens.lens (\DescribeAssessmentTemplatesResponse' {failedItems} -> failedItems) (\s@DescribeAssessmentTemplatesResponse' {} a -> s {failedItems = a} :: DescribeAssessmentTemplatesResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    DescribeAssessmentTemplatesResponse
