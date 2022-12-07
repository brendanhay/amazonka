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
-- Module      : Amazonka.Inspector.CreateExclusionsPreview
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the generation of an exclusions preview for the specified
-- assessment template. The exclusions preview lists the potential
-- exclusions (ExclusionPreview) that Inspector can detect before it runs
-- the assessment.
module Amazonka.Inspector.CreateExclusionsPreview
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateExclusionsPreview' smart constructor.
data CreateExclusionsPreview = CreateExclusionsPreview'
  { -- | The ARN that specifies the assessment template for which you want to
    -- create an exclusions preview.
    assessmentTemplateArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  CreateExclusionsPreview
newCreateExclusionsPreview pAssessmentTemplateArn_ =
  CreateExclusionsPreview'
    { assessmentTemplateArn =
        pAssessmentTemplateArn_
    }

-- | The ARN that specifies the assessment template for which you want to
-- create an exclusions preview.
createExclusionsPreview_assessmentTemplateArn :: Lens.Lens' CreateExclusionsPreview Prelude.Text
createExclusionsPreview_assessmentTemplateArn = Lens.lens (\CreateExclusionsPreview' {assessmentTemplateArn} -> assessmentTemplateArn) (\s@CreateExclusionsPreview' {} a -> s {assessmentTemplateArn = a} :: CreateExclusionsPreview)

instance Core.AWSRequest CreateExclusionsPreview where
  type
    AWSResponse CreateExclusionsPreview =
      CreateExclusionsPreviewResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateExclusionsPreviewResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "previewToken")
      )

instance Prelude.Hashable CreateExclusionsPreview where
  hashWithSalt _salt CreateExclusionsPreview' {..} =
    _salt `Prelude.hashWithSalt` assessmentTemplateArn

instance Prelude.NFData CreateExclusionsPreview where
  rnf CreateExclusionsPreview' {..} =
    Prelude.rnf assessmentTemplateArn

instance Data.ToHeaders CreateExclusionsPreview where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.CreateExclusionsPreview" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateExclusionsPreview where
  toJSON CreateExclusionsPreview' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "assessmentTemplateArn"
                  Data..= assessmentTemplateArn
              )
          ]
      )

instance Data.ToPath CreateExclusionsPreview where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateExclusionsPreview where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateExclusionsPreviewResponse' smart constructor.
data CreateExclusionsPreviewResponse = CreateExclusionsPreviewResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Specifies the unique identifier of the requested exclusions preview. You
    -- can use the unique identifier to retrieve the exclusions preview when
    -- running the GetExclusionsPreview API.
    previewToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'previewToken'
  Prelude.Text ->
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
createExclusionsPreviewResponse_httpStatus :: Lens.Lens' CreateExclusionsPreviewResponse Prelude.Int
createExclusionsPreviewResponse_httpStatus = Lens.lens (\CreateExclusionsPreviewResponse' {httpStatus} -> httpStatus) (\s@CreateExclusionsPreviewResponse' {} a -> s {httpStatus = a} :: CreateExclusionsPreviewResponse)

-- | Specifies the unique identifier of the requested exclusions preview. You
-- can use the unique identifier to retrieve the exclusions preview when
-- running the GetExclusionsPreview API.
createExclusionsPreviewResponse_previewToken :: Lens.Lens' CreateExclusionsPreviewResponse Prelude.Text
createExclusionsPreviewResponse_previewToken = Lens.lens (\CreateExclusionsPreviewResponse' {previewToken} -> previewToken) (\s@CreateExclusionsPreviewResponse' {} a -> s {previewToken = a} :: CreateExclusionsPreviewResponse)

instance
  Prelude.NFData
    CreateExclusionsPreviewResponse
  where
  rnf CreateExclusionsPreviewResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf previewToken
