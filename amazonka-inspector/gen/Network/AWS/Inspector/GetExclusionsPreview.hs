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
-- Module      : Network.AWS.Inspector.GetExclusionsPreview
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the exclusions preview (a list of ExclusionPreview objects)
-- specified by the preview token. You can obtain the preview token by
-- running the CreateExclusionsPreview API.
module Network.AWS.Inspector.GetExclusionsPreview
  ( -- * Creating a Request
    GetExclusionsPreview (..),
    newGetExclusionsPreview,

    -- * Request Lenses
    getExclusionsPreview_nextToken,
    getExclusionsPreview_maxResults,
    getExclusionsPreview_locale,
    getExclusionsPreview_assessmentTemplateArn,
    getExclusionsPreview_previewToken,

    -- * Destructuring the Response
    GetExclusionsPreviewResponse (..),
    newGetExclusionsPreviewResponse,

    -- * Response Lenses
    getExclusionsPreviewResponse_nextToken,
    getExclusionsPreviewResponse_exclusionPreviews,
    getExclusionsPreviewResponse_httpStatus,
    getExclusionsPreviewResponse_previewStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetExclusionsPreview' smart constructor.
data GetExclusionsPreview = GetExclusionsPreview'
  { -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the
    -- GetExclusionsPreviewRequest action. Subsequent calls to the action fill
    -- nextToken in the request with the value of nextToken from the previous
    -- response to continue listing data.
    nextToken :: Core.Maybe Core.Text,
    -- | You can use this parameter to indicate the maximum number of items you
    -- want in the response. The default value is 100. The maximum value is
    -- 500.
    maxResults :: Core.Maybe Core.Int,
    -- | The locale into which you want to translate the exclusion\'s title,
    -- description, and recommendation.
    locale :: Core.Maybe Locale,
    -- | The ARN that specifies the assessment template for which the exclusions
    -- preview was requested.
    assessmentTemplateArn :: Core.Text,
    -- | The unique identifier associated of the exclusions preview.
    previewToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetExclusionsPreview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getExclusionsPreview_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- GetExclusionsPreviewRequest action. Subsequent calls to the action fill
-- nextToken in the request with the value of nextToken from the previous
-- response to continue listing data.
--
-- 'maxResults', 'getExclusionsPreview_maxResults' - You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 100. The maximum value is
-- 500.
--
-- 'locale', 'getExclusionsPreview_locale' - The locale into which you want to translate the exclusion\'s title,
-- description, and recommendation.
--
-- 'assessmentTemplateArn', 'getExclusionsPreview_assessmentTemplateArn' - The ARN that specifies the assessment template for which the exclusions
-- preview was requested.
--
-- 'previewToken', 'getExclusionsPreview_previewToken' - The unique identifier associated of the exclusions preview.
newGetExclusionsPreview ::
  -- | 'assessmentTemplateArn'
  Core.Text ->
  -- | 'previewToken'
  Core.Text ->
  GetExclusionsPreview
newGetExclusionsPreview
  pAssessmentTemplateArn_
  pPreviewToken_ =
    GetExclusionsPreview'
      { nextToken = Core.Nothing,
        maxResults = Core.Nothing,
        locale = Core.Nothing,
        assessmentTemplateArn = pAssessmentTemplateArn_,
        previewToken = pPreviewToken_
      }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- GetExclusionsPreviewRequest action. Subsequent calls to the action fill
-- nextToken in the request with the value of nextToken from the previous
-- response to continue listing data.
getExclusionsPreview_nextToken :: Lens.Lens' GetExclusionsPreview (Core.Maybe Core.Text)
getExclusionsPreview_nextToken = Lens.lens (\GetExclusionsPreview' {nextToken} -> nextToken) (\s@GetExclusionsPreview' {} a -> s {nextToken = a} :: GetExclusionsPreview)

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 100. The maximum value is
-- 500.
getExclusionsPreview_maxResults :: Lens.Lens' GetExclusionsPreview (Core.Maybe Core.Int)
getExclusionsPreview_maxResults = Lens.lens (\GetExclusionsPreview' {maxResults} -> maxResults) (\s@GetExclusionsPreview' {} a -> s {maxResults = a} :: GetExclusionsPreview)

-- | The locale into which you want to translate the exclusion\'s title,
-- description, and recommendation.
getExclusionsPreview_locale :: Lens.Lens' GetExclusionsPreview (Core.Maybe Locale)
getExclusionsPreview_locale = Lens.lens (\GetExclusionsPreview' {locale} -> locale) (\s@GetExclusionsPreview' {} a -> s {locale = a} :: GetExclusionsPreview)

-- | The ARN that specifies the assessment template for which the exclusions
-- preview was requested.
getExclusionsPreview_assessmentTemplateArn :: Lens.Lens' GetExclusionsPreview Core.Text
getExclusionsPreview_assessmentTemplateArn = Lens.lens (\GetExclusionsPreview' {assessmentTemplateArn} -> assessmentTemplateArn) (\s@GetExclusionsPreview' {} a -> s {assessmentTemplateArn = a} :: GetExclusionsPreview)

-- | The unique identifier associated of the exclusions preview.
getExclusionsPreview_previewToken :: Lens.Lens' GetExclusionsPreview Core.Text
getExclusionsPreview_previewToken = Lens.lens (\GetExclusionsPreview' {previewToken} -> previewToken) (\s@GetExclusionsPreview' {} a -> s {previewToken = a} :: GetExclusionsPreview)

instance Core.AWSRequest GetExclusionsPreview where
  type
    AWSResponse GetExclusionsPreview =
      GetExclusionsPreviewResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExclusionsPreviewResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "exclusionPreviews" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "previewStatus")
      )

instance Core.Hashable GetExclusionsPreview

instance Core.NFData GetExclusionsPreview

instance Core.ToHeaders GetExclusionsPreview where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.GetExclusionsPreview" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetExclusionsPreview where
  toJSON GetExclusionsPreview' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("locale" Core..=) Core.<$> locale,
            Core.Just
              ( "assessmentTemplateArn"
                  Core..= assessmentTemplateArn
              ),
            Core.Just ("previewToken" Core..= previewToken)
          ]
      )

instance Core.ToPath GetExclusionsPreview where
  toPath = Core.const "/"

instance Core.ToQuery GetExclusionsPreview where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetExclusionsPreviewResponse' smart constructor.
data GetExclusionsPreviewResponse = GetExclusionsPreviewResponse'
  { -- | When a response is generated, if there is more data to be listed, this
    -- parameters is present in the response and contains the value to use for
    -- the nextToken parameter in a subsequent pagination request. If there is
    -- no more data to be listed, this parameter is set to null.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the exclusions included in the preview.
    exclusionPreviews :: Core.Maybe [ExclusionPreview],
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Specifies the status of the request to generate an exclusions preview.
    previewStatus :: PreviewStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetExclusionsPreviewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getExclusionsPreviewResponse_nextToken' - When a response is generated, if there is more data to be listed, this
-- parameters is present in the response and contains the value to use for
-- the nextToken parameter in a subsequent pagination request. If there is
-- no more data to be listed, this parameter is set to null.
--
-- 'exclusionPreviews', 'getExclusionsPreviewResponse_exclusionPreviews' - Information about the exclusions included in the preview.
--
-- 'httpStatus', 'getExclusionsPreviewResponse_httpStatus' - The response's http status code.
--
-- 'previewStatus', 'getExclusionsPreviewResponse_previewStatus' - Specifies the status of the request to generate an exclusions preview.
newGetExclusionsPreviewResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'previewStatus'
  PreviewStatus ->
  GetExclusionsPreviewResponse
newGetExclusionsPreviewResponse
  pHttpStatus_
  pPreviewStatus_ =
    GetExclusionsPreviewResponse'
      { nextToken =
          Core.Nothing,
        exclusionPreviews = Core.Nothing,
        httpStatus = pHttpStatus_,
        previewStatus = pPreviewStatus_
      }

-- | When a response is generated, if there is more data to be listed, this
-- parameters is present in the response and contains the value to use for
-- the nextToken parameter in a subsequent pagination request. If there is
-- no more data to be listed, this parameter is set to null.
getExclusionsPreviewResponse_nextToken :: Lens.Lens' GetExclusionsPreviewResponse (Core.Maybe Core.Text)
getExclusionsPreviewResponse_nextToken = Lens.lens (\GetExclusionsPreviewResponse' {nextToken} -> nextToken) (\s@GetExclusionsPreviewResponse' {} a -> s {nextToken = a} :: GetExclusionsPreviewResponse)

-- | Information about the exclusions included in the preview.
getExclusionsPreviewResponse_exclusionPreviews :: Lens.Lens' GetExclusionsPreviewResponse (Core.Maybe [ExclusionPreview])
getExclusionsPreviewResponse_exclusionPreviews = Lens.lens (\GetExclusionsPreviewResponse' {exclusionPreviews} -> exclusionPreviews) (\s@GetExclusionsPreviewResponse' {} a -> s {exclusionPreviews = a} :: GetExclusionsPreviewResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getExclusionsPreviewResponse_httpStatus :: Lens.Lens' GetExclusionsPreviewResponse Core.Int
getExclusionsPreviewResponse_httpStatus = Lens.lens (\GetExclusionsPreviewResponse' {httpStatus} -> httpStatus) (\s@GetExclusionsPreviewResponse' {} a -> s {httpStatus = a} :: GetExclusionsPreviewResponse)

-- | Specifies the status of the request to generate an exclusions preview.
getExclusionsPreviewResponse_previewStatus :: Lens.Lens' GetExclusionsPreviewResponse PreviewStatus
getExclusionsPreviewResponse_previewStatus = Lens.lens (\GetExclusionsPreviewResponse' {previewStatus} -> previewStatus) (\s@GetExclusionsPreviewResponse' {} a -> s {previewStatus = a} :: GetExclusionsPreviewResponse)

instance Core.NFData GetExclusionsPreviewResponse
