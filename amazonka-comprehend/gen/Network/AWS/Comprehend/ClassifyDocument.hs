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
-- Module      : Network.AWS.Comprehend.ClassifyDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new document classification request to analyze a single
-- document in real-time, using a previously created and trained custom
-- model and an endpoint.
module Network.AWS.Comprehend.ClassifyDocument
  ( -- * Creating a Request
    ClassifyDocument (..),
    newClassifyDocument,

    -- * Request Lenses
    classifyDocument_text,
    classifyDocument_endpointArn,

    -- * Destructuring the Response
    ClassifyDocumentResponse (..),
    newClassifyDocumentResponse,

    -- * Response Lenses
    classifyDocumentResponse_classes,
    classifyDocumentResponse_labels,
    classifyDocumentResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newClassifyDocument' smart constructor.
data ClassifyDocument = ClassifyDocument'
  { -- | The document text to be analyzed.
    text :: Core.Sensitive Core.Text,
    -- | The Amazon Resource Number (ARN) of the endpoint.
    endpointArn :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClassifyDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'classifyDocument_text' - The document text to be analyzed.
--
-- 'endpointArn', 'classifyDocument_endpointArn' - The Amazon Resource Number (ARN) of the endpoint.
newClassifyDocument ::
  -- | 'text'
  Core.Text ->
  -- | 'endpointArn'
  Core.Text ->
  ClassifyDocument
newClassifyDocument pText_ pEndpointArn_ =
  ClassifyDocument'
    { text =
        Core._Sensitive Lens.# pText_,
      endpointArn = pEndpointArn_
    }

-- | The document text to be analyzed.
classifyDocument_text :: Lens.Lens' ClassifyDocument Core.Text
classifyDocument_text = Lens.lens (\ClassifyDocument' {text} -> text) (\s@ClassifyDocument' {} a -> s {text = a} :: ClassifyDocument) Core.. Core._Sensitive

-- | The Amazon Resource Number (ARN) of the endpoint.
classifyDocument_endpointArn :: Lens.Lens' ClassifyDocument Core.Text
classifyDocument_endpointArn = Lens.lens (\ClassifyDocument' {endpointArn} -> endpointArn) (\s@ClassifyDocument' {} a -> s {endpointArn = a} :: ClassifyDocument)

instance Core.AWSRequest ClassifyDocument where
  type
    AWSResponse ClassifyDocument =
      ClassifyDocumentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ClassifyDocumentResponse'
            Core.<$> (x Core..?> "Classes" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Labels" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ClassifyDocument

instance Core.NFData ClassifyDocument

instance Core.ToHeaders ClassifyDocument where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.ClassifyDocument" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ClassifyDocument where
  toJSON ClassifyDocument' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Text" Core..= text),
            Core.Just ("EndpointArn" Core..= endpointArn)
          ]
      )

instance Core.ToPath ClassifyDocument where
  toPath = Core.const "/"

instance Core.ToQuery ClassifyDocument where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newClassifyDocumentResponse' smart constructor.
data ClassifyDocumentResponse = ClassifyDocumentResponse'
  { -- | The classes used by the document being analyzed. These are used for
    -- multi-class trained models. Individual classes are mutually exclusive
    -- and each document is expected to have only a single class assigned to
    -- it. For example, an animal can be a dog or a cat, but not both at the
    -- same time.
    classes :: Core.Maybe [DocumentClass],
    -- | The labels used the document being analyzed. These are used for
    -- multi-label trained models. Individual labels represent different
    -- categories that are related in some manner and are not mutually
    -- exclusive. For example, a movie can be just an action movie, or it can
    -- be an action movie, a science fiction movie, and a comedy, all at the
    -- same time.
    labels :: Core.Maybe [DocumentLabel],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClassifyDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'classes', 'classifyDocumentResponse_classes' - The classes used by the document being analyzed. These are used for
-- multi-class trained models. Individual classes are mutually exclusive
-- and each document is expected to have only a single class assigned to
-- it. For example, an animal can be a dog or a cat, but not both at the
-- same time.
--
-- 'labels', 'classifyDocumentResponse_labels' - The labels used the document being analyzed. These are used for
-- multi-label trained models. Individual labels represent different
-- categories that are related in some manner and are not mutually
-- exclusive. For example, a movie can be just an action movie, or it can
-- be an action movie, a science fiction movie, and a comedy, all at the
-- same time.
--
-- 'httpStatus', 'classifyDocumentResponse_httpStatus' - The response's http status code.
newClassifyDocumentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ClassifyDocumentResponse
newClassifyDocumentResponse pHttpStatus_ =
  ClassifyDocumentResponse'
    { classes = Core.Nothing,
      labels = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The classes used by the document being analyzed. These are used for
-- multi-class trained models. Individual classes are mutually exclusive
-- and each document is expected to have only a single class assigned to
-- it. For example, an animal can be a dog or a cat, but not both at the
-- same time.
classifyDocumentResponse_classes :: Lens.Lens' ClassifyDocumentResponse (Core.Maybe [DocumentClass])
classifyDocumentResponse_classes = Lens.lens (\ClassifyDocumentResponse' {classes} -> classes) (\s@ClassifyDocumentResponse' {} a -> s {classes = a} :: ClassifyDocumentResponse) Core.. Lens.mapping Lens._Coerce

-- | The labels used the document being analyzed. These are used for
-- multi-label trained models. Individual labels represent different
-- categories that are related in some manner and are not mutually
-- exclusive. For example, a movie can be just an action movie, or it can
-- be an action movie, a science fiction movie, and a comedy, all at the
-- same time.
classifyDocumentResponse_labels :: Lens.Lens' ClassifyDocumentResponse (Core.Maybe [DocumentLabel])
classifyDocumentResponse_labels = Lens.lens (\ClassifyDocumentResponse' {labels} -> labels) (\s@ClassifyDocumentResponse' {} a -> s {labels = a} :: ClassifyDocumentResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
classifyDocumentResponse_httpStatus :: Lens.Lens' ClassifyDocumentResponse Core.Int
classifyDocumentResponse_httpStatus = Lens.lens (\ClassifyDocumentResponse' {httpStatus} -> httpStatus) (\s@ClassifyDocumentResponse' {} a -> s {httpStatus = a} :: ClassifyDocumentResponse)

instance Core.NFData ClassifyDocumentResponse
