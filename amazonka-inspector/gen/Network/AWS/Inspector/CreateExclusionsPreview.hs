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
-- Module      : Network.AWS.Inspector.CreateExclusionsPreview
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the generation of an exclusions preview for the specified
-- assessment template. The exclusions preview lists the potential
-- exclusions (ExclusionPreview) that Inspector can detect before it runs
-- the assessment.
module Network.AWS.Inspector.CreateExclusionsPreview
  ( -- * Creating a Request
    CreateExclusionsPreview (..),
    newCreateExclusionsPreview,

    -- * Request Lenses
    createExclusionsPreview_assessmentTemplateArn,

    -- * Destructuring the Response
    CreateExclusionsPreviewResponse (..),
    newCreateExclusionsPreviewResponse,

    -- * Response Lenses
    createExclusionsPreviewResponse_httpStatus,
    createExclusionsPreviewResponse_previewToken,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateExclusionsPreview' smart constructor.
data CreateExclusionsPreview = CreateExclusionsPreview'
  { -- | The ARN that specifies the assessment template for which you want to
    -- create an exclusions preview.
    assessmentTemplateArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateExclusionsPreview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentTemplateArn', 'createExclusionsPreview_assessmentTemplateArn' - The ARN that specifies the assessment template for which you want to
-- create an exclusions preview.
newCreateExclusionsPreview ::
  -- | 'assessmentTemplateArn'
  Core.Text ->
  CreateExclusionsPreview
newCreateExclusionsPreview pAssessmentTemplateArn_ =
  CreateExclusionsPreview'
    { assessmentTemplateArn =
        pAssessmentTemplateArn_
    }

-- | The ARN that specifies the assessment template for which you want to
-- create an exclusions preview.
createExclusionsPreview_assessmentTemplateArn :: Lens.Lens' CreateExclusionsPreview Core.Text
createExclusionsPreview_assessmentTemplateArn = Lens.lens (\CreateExclusionsPreview' {assessmentTemplateArn} -> assessmentTemplateArn) (\s@CreateExclusionsPreview' {} a -> s {assessmentTemplateArn = a} :: CreateExclusionsPreview)

instance Core.AWSRequest CreateExclusionsPreview where
  type
    AWSResponse CreateExclusionsPreview =
      CreateExclusionsPreviewResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateExclusionsPreviewResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "previewToken")
      )

instance Core.Hashable CreateExclusionsPreview

instance Core.NFData CreateExclusionsPreview

instance Core.ToHeaders CreateExclusionsPreview where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.CreateExclusionsPreview" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateExclusionsPreview where
  toJSON CreateExclusionsPreview' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "assessmentTemplateArn"
                  Core..= assessmentTemplateArn
              )
          ]
      )

instance Core.ToPath CreateExclusionsPreview where
  toPath = Core.const "/"

instance Core.ToQuery CreateExclusionsPreview where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateExclusionsPreviewResponse' smart constructor.
data CreateExclusionsPreviewResponse = CreateExclusionsPreviewResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Specifies the unique identifier of the requested exclusions preview. You
    -- can use the unique identifier to retrieve the exclusions preview when
    -- running the GetExclusionsPreview API.
    previewToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateExclusionsPreviewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createExclusionsPreviewResponse_httpStatus' - The response's http status code.
--
-- 'previewToken', 'createExclusionsPreviewResponse_previewToken' - Specifies the unique identifier of the requested exclusions preview. You
-- can use the unique identifier to retrieve the exclusions preview when
-- running the GetExclusionsPreview API.
newCreateExclusionsPreviewResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'previewToken'
  Core.Text ->
  CreateExclusionsPreviewResponse
newCreateExclusionsPreviewResponse
  pHttpStatus_
  pPreviewToken_ =
    CreateExclusionsPreviewResponse'
      { httpStatus =
          pHttpStatus_,
        previewToken = pPreviewToken_
      }

-- | The response's http status code.
createExclusionsPreviewResponse_httpStatus :: Lens.Lens' CreateExclusionsPreviewResponse Core.Int
createExclusionsPreviewResponse_httpStatus = Lens.lens (\CreateExclusionsPreviewResponse' {httpStatus} -> httpStatus) (\s@CreateExclusionsPreviewResponse' {} a -> s {httpStatus = a} :: CreateExclusionsPreviewResponse)

-- | Specifies the unique identifier of the requested exclusions preview. You
-- can use the unique identifier to retrieve the exclusions preview when
-- running the GetExclusionsPreview API.
createExclusionsPreviewResponse_previewToken :: Lens.Lens' CreateExclusionsPreviewResponse Core.Text
createExclusionsPreviewResponse_previewToken = Lens.lens (\CreateExclusionsPreviewResponse' {previewToken} -> previewToken) (\s@CreateExclusionsPreviewResponse' {} a -> s {previewToken = a} :: CreateExclusionsPreviewResponse)

instance Core.NFData CreateExclusionsPreviewResponse
