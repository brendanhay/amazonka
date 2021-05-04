{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateExclusionsPreview' smart constructor.
data CreateExclusionsPreview = CreateExclusionsPreview'
  { -- | The ARN that specifies the assessment template for which you want to
    -- create an exclusions preview.
    assessmentTemplateArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest CreateExclusionsPreview where
  type
    Rs CreateExclusionsPreview =
      CreateExclusionsPreviewResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateExclusionsPreviewResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "previewToken")
      )

instance Prelude.Hashable CreateExclusionsPreview

instance Prelude.NFData CreateExclusionsPreview

instance Prelude.ToHeaders CreateExclusionsPreview where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "InspectorService.CreateExclusionsPreview" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateExclusionsPreview where
  toJSON CreateExclusionsPreview' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "assessmentTemplateArn"
                  Prelude..= assessmentTemplateArn
              )
          ]
      )

instance Prelude.ToPath CreateExclusionsPreview where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateExclusionsPreview where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
