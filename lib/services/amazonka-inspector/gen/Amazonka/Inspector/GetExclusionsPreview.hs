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
-- Module      : Amazonka.Inspector.GetExclusionsPreview
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the exclusions preview (a list of ExclusionPreview objects)
-- specified by the preview token. You can obtain the preview token by
-- running the CreateExclusionsPreview API.
module Amazonka.Inspector.GetExclusionsPreview
  ( -- * Creating a Request
    GetExclusionsPreview (..),
    newGetExclusionsPreview,

    -- * Request Lenses
    getExclusionsPreview_locale,
    getExclusionsPreview_maxResults,
    getExclusionsPreview_nextToken,
    getExclusionsPreview_assessmentTemplateArn,
    getExclusionsPreview_previewToken,

    -- * Destructuring the Response
    GetExclusionsPreviewResponse (..),
    newGetExclusionsPreviewResponse,

    -- * Response Lenses
    getExclusionsPreviewResponse_exclusionPreviews,
    getExclusionsPreviewResponse_nextToken,
    getExclusionsPreviewResponse_httpStatus,
    getExclusionsPreviewResponse_previewStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetExclusionsPreview' smart constructor.
data GetExclusionsPreview = GetExclusionsPreview'
  { -- | The locale into which you want to translate the exclusion\'s title,
    -- description, and recommendation.
    locale :: Prelude.Maybe Locale,
    -- | You can use this parameter to indicate the maximum number of items you
    -- want in the response. The default value is 100. The maximum value is
    -- 500.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the
    -- GetExclusionsPreviewRequest action. Subsequent calls to the action fill
    -- nextToken in the request with the value of nextToken from the previous
    -- response to continue listing data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN that specifies the assessment template for which the exclusions
    -- preview was requested.
    assessmentTemplateArn :: Prelude.Text,
    -- | The unique identifier associated of the exclusions preview.
    previewToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExclusionsPreview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locale', 'getExclusionsPreview_locale' - The locale into which you want to translate the exclusion\'s title,
-- description, and recommendation.
--
-- 'maxResults', 'getExclusionsPreview_maxResults' - You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 100. The maximum value is
-- 500.
--
-- 'nextToken', 'getExclusionsPreview_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- GetExclusionsPreviewRequest action. Subsequent calls to the action fill
-- nextToken in the request with the value of nextToken from the previous
-- response to continue listing data.
--
-- 'assessmentTemplateArn', 'getExclusionsPreview_assessmentTemplateArn' - The ARN that specifies the assessment template for which the exclusions
-- preview was requested.
--
-- 'previewToken', 'getExclusionsPreview_previewToken' - The unique identifier associated of the exclusions preview.
newGetExclusionsPreview ::
  -- | 'assessmentTemplateArn'
  Prelude.Text ->
  -- | 'previewToken'
  Prelude.Text ->
  GetExclusionsPreview
newGetExclusionsPreview
  pAssessmentTemplateArn_
  pPreviewToken_ =
    GetExclusionsPreview'
      { locale = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        assessmentTemplateArn = pAssessmentTemplateArn_,
        previewToken = pPreviewToken_
      }

-- | The locale into which you want to translate the exclusion\'s title,
-- description, and recommendation.
getExclusionsPreview_locale :: Lens.Lens' GetExclusionsPreview (Prelude.Maybe Locale)
getExclusionsPreview_locale = Lens.lens (\GetExclusionsPreview' {locale} -> locale) (\s@GetExclusionsPreview' {} a -> s {locale = a} :: GetExclusionsPreview)

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 100. The maximum value is
-- 500.
getExclusionsPreview_maxResults :: Lens.Lens' GetExclusionsPreview (Prelude.Maybe Prelude.Int)
getExclusionsPreview_maxResults = Lens.lens (\GetExclusionsPreview' {maxResults} -> maxResults) (\s@GetExclusionsPreview' {} a -> s {maxResults = a} :: GetExclusionsPreview)

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- GetExclusionsPreviewRequest action. Subsequent calls to the action fill
-- nextToken in the request with the value of nextToken from the previous
-- response to continue listing data.
getExclusionsPreview_nextToken :: Lens.Lens' GetExclusionsPreview (Prelude.Maybe Prelude.Text)
getExclusionsPreview_nextToken = Lens.lens (\GetExclusionsPreview' {nextToken} -> nextToken) (\s@GetExclusionsPreview' {} a -> s {nextToken = a} :: GetExclusionsPreview)

-- | The ARN that specifies the assessment template for which the exclusions
-- preview was requested.
getExclusionsPreview_assessmentTemplateArn :: Lens.Lens' GetExclusionsPreview Prelude.Text
getExclusionsPreview_assessmentTemplateArn = Lens.lens (\GetExclusionsPreview' {assessmentTemplateArn} -> assessmentTemplateArn) (\s@GetExclusionsPreview' {} a -> s {assessmentTemplateArn = a} :: GetExclusionsPreview)

-- | The unique identifier associated of the exclusions preview.
getExclusionsPreview_previewToken :: Lens.Lens' GetExclusionsPreview Prelude.Text
getExclusionsPreview_previewToken = Lens.lens (\GetExclusionsPreview' {previewToken} -> previewToken) (\s@GetExclusionsPreview' {} a -> s {previewToken = a} :: GetExclusionsPreview)

instance Core.AWSRequest GetExclusionsPreview where
  type
    AWSResponse GetExclusionsPreview =
      GetExclusionsPreviewResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExclusionsPreviewResponse'
            Prelude.<$> ( x Data..?> "exclusionPreviews"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "previewStatus")
      )

instance Prelude.Hashable GetExclusionsPreview where
  hashWithSalt _salt GetExclusionsPreview' {..} =
    _salt `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` assessmentTemplateArn
      `Prelude.hashWithSalt` previewToken

instance Prelude.NFData GetExclusionsPreview where
  rnf GetExclusionsPreview' {..} =
    Prelude.rnf locale
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf assessmentTemplateArn
      `Prelude.seq` Prelude.rnf previewToken

instance Data.ToHeaders GetExclusionsPreview where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.GetExclusionsPreview" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetExclusionsPreview where
  toJSON GetExclusionsPreview' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("locale" Data..=) Prelude.<$> locale,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ( "assessmentTemplateArn"
                  Data..= assessmentTemplateArn
              ),
            Prelude.Just ("previewToken" Data..= previewToken)
          ]
      )

instance Data.ToPath GetExclusionsPreview where
  toPath = Prelude.const "/"

instance Data.ToQuery GetExclusionsPreview where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetExclusionsPreviewResponse' smart constructor.
data GetExclusionsPreviewResponse = GetExclusionsPreviewResponse'
  { -- | Information about the exclusions included in the preview.
    exclusionPreviews :: Prelude.Maybe [ExclusionPreview],
    -- | When a response is generated, if there is more data to be listed, this
    -- parameters is present in the response and contains the value to use for
    -- the nextToken parameter in a subsequent pagination request. If there is
    -- no more data to be listed, this parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Specifies the status of the request to generate an exclusions preview.
    previewStatus :: PreviewStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExclusionsPreviewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exclusionPreviews', 'getExclusionsPreviewResponse_exclusionPreviews' - Information about the exclusions included in the preview.
--
-- 'nextToken', 'getExclusionsPreviewResponse_nextToken' - When a response is generated, if there is more data to be listed, this
-- parameters is present in the response and contains the value to use for
-- the nextToken parameter in a subsequent pagination request. If there is
-- no more data to be listed, this parameter is set to null.
--
-- 'httpStatus', 'getExclusionsPreviewResponse_httpStatus' - The response's http status code.
--
-- 'previewStatus', 'getExclusionsPreviewResponse_previewStatus' - Specifies the status of the request to generate an exclusions preview.
newGetExclusionsPreviewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'previewStatus'
  PreviewStatus ->
  GetExclusionsPreviewResponse
newGetExclusionsPreviewResponse
  pHttpStatus_
  pPreviewStatus_ =
    GetExclusionsPreviewResponse'
      { exclusionPreviews =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        previewStatus = pPreviewStatus_
      }

-- | Information about the exclusions included in the preview.
getExclusionsPreviewResponse_exclusionPreviews :: Lens.Lens' GetExclusionsPreviewResponse (Prelude.Maybe [ExclusionPreview])
getExclusionsPreviewResponse_exclusionPreviews = Lens.lens (\GetExclusionsPreviewResponse' {exclusionPreviews} -> exclusionPreviews) (\s@GetExclusionsPreviewResponse' {} a -> s {exclusionPreviews = a} :: GetExclusionsPreviewResponse) Prelude.. Lens.mapping Lens.coerced

-- | When a response is generated, if there is more data to be listed, this
-- parameters is present in the response and contains the value to use for
-- the nextToken parameter in a subsequent pagination request. If there is
-- no more data to be listed, this parameter is set to null.
getExclusionsPreviewResponse_nextToken :: Lens.Lens' GetExclusionsPreviewResponse (Prelude.Maybe Prelude.Text)
getExclusionsPreviewResponse_nextToken = Lens.lens (\GetExclusionsPreviewResponse' {nextToken} -> nextToken) (\s@GetExclusionsPreviewResponse' {} a -> s {nextToken = a} :: GetExclusionsPreviewResponse)

-- | The response's http status code.
getExclusionsPreviewResponse_httpStatus :: Lens.Lens' GetExclusionsPreviewResponse Prelude.Int
getExclusionsPreviewResponse_httpStatus = Lens.lens (\GetExclusionsPreviewResponse' {httpStatus} -> httpStatus) (\s@GetExclusionsPreviewResponse' {} a -> s {httpStatus = a} :: GetExclusionsPreviewResponse)

-- | Specifies the status of the request to generate an exclusions preview.
getExclusionsPreviewResponse_previewStatus :: Lens.Lens' GetExclusionsPreviewResponse PreviewStatus
getExclusionsPreviewResponse_previewStatus = Lens.lens (\GetExclusionsPreviewResponse' {previewStatus} -> previewStatus) (\s@GetExclusionsPreviewResponse' {} a -> s {previewStatus = a} :: GetExclusionsPreviewResponse)

instance Prelude.NFData GetExclusionsPreviewResponse where
  rnf GetExclusionsPreviewResponse' {..} =
    Prelude.rnf exclusionPreviews
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf previewStatus
