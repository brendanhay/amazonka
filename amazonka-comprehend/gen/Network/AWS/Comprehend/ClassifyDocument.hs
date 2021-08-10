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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newClassifyDocument' smart constructor.
data ClassifyDocument = ClassifyDocument'
  { -- | The document text to be analyzed.
    text :: Core.Sensitive Prelude.Text,
    -- | The Amazon Resource Number (ARN) of the endpoint.
    endpointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'endpointArn'
  Prelude.Text ->
  ClassifyDocument
newClassifyDocument pText_ pEndpointArn_ =
  ClassifyDocument'
    { text =
        Core._Sensitive Lens.# pText_,
      endpointArn = pEndpointArn_
    }

-- | The document text to be analyzed.
classifyDocument_text :: Lens.Lens' ClassifyDocument Prelude.Text
classifyDocument_text = Lens.lens (\ClassifyDocument' {text} -> text) (\s@ClassifyDocument' {} a -> s {text = a} :: ClassifyDocument) Prelude.. Core._Sensitive

-- | The Amazon Resource Number (ARN) of the endpoint.
classifyDocument_endpointArn :: Lens.Lens' ClassifyDocument Prelude.Text
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
            Prelude.<$> (x Core..?> "Classes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Labels" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ClassifyDocument

instance Prelude.NFData ClassifyDocument

instance Core.ToHeaders ClassifyDocument where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.ClassifyDocument" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ClassifyDocument where
  toJSON ClassifyDocument' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Text" Core..= text),
            Prelude.Just ("EndpointArn" Core..= endpointArn)
          ]
      )

instance Core.ToPath ClassifyDocument where
  toPath = Prelude.const "/"

instance Core.ToQuery ClassifyDocument where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newClassifyDocumentResponse' smart constructor.
data ClassifyDocumentResponse = ClassifyDocumentResponse'
  { -- | The classes used by the document being analyzed. These are used for
    -- multi-class trained models. Individual classes are mutually exclusive
    -- and each document is expected to have only a single class assigned to
    -- it. For example, an animal can be a dog or a cat, but not both at the
    -- same time.
    classes :: Prelude.Maybe [DocumentClass],
    -- | The labels used the document being analyzed. These are used for
    -- multi-label trained models. Individual labels represent different
    -- categories that are related in some manner and are not mutually
    -- exclusive. For example, a movie can be just an action movie, or it can
    -- be an action movie, a science fiction movie, and a comedy, all at the
    -- same time.
    labels :: Prelude.Maybe [DocumentLabel],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ClassifyDocumentResponse
newClassifyDocumentResponse pHttpStatus_ =
  ClassifyDocumentResponse'
    { classes =
        Prelude.Nothing,
      labels = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The classes used by the document being analyzed. These are used for
-- multi-class trained models. Individual classes are mutually exclusive
-- and each document is expected to have only a single class assigned to
-- it. For example, an animal can be a dog or a cat, but not both at the
-- same time.
classifyDocumentResponse_classes :: Lens.Lens' ClassifyDocumentResponse (Prelude.Maybe [DocumentClass])
classifyDocumentResponse_classes = Lens.lens (\ClassifyDocumentResponse' {classes} -> classes) (\s@ClassifyDocumentResponse' {} a -> s {classes = a} :: ClassifyDocumentResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The labels used the document being analyzed. These are used for
-- multi-label trained models. Individual labels represent different
-- categories that are related in some manner and are not mutually
-- exclusive. For example, a movie can be just an action movie, or it can
-- be an action movie, a science fiction movie, and a comedy, all at the
-- same time.
classifyDocumentResponse_labels :: Lens.Lens' ClassifyDocumentResponse (Prelude.Maybe [DocumentLabel])
classifyDocumentResponse_labels = Lens.lens (\ClassifyDocumentResponse' {labels} -> labels) (\s@ClassifyDocumentResponse' {} a -> s {labels = a} :: ClassifyDocumentResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
classifyDocumentResponse_httpStatus :: Lens.Lens' ClassifyDocumentResponse Prelude.Int
classifyDocumentResponse_httpStatus = Lens.lens (\ClassifyDocumentResponse' {httpStatus} -> httpStatus) (\s@ClassifyDocumentResponse' {} a -> s {httpStatus = a} :: ClassifyDocumentResponse)

instance Prelude.NFData ClassifyDocumentResponse
