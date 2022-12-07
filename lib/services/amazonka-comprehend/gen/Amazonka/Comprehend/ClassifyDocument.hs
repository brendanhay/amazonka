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
-- Module      : Amazonka.Comprehend.ClassifyDocument
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new document classification request to analyze a single
-- document in real-time, using a previously created and trained custom
-- model and an endpoint.
module Amazonka.Comprehend.ClassifyDocument
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

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newClassifyDocument' smart constructor.
data ClassifyDocument = ClassifyDocument'
  { -- | The document text to be analyzed.
    text :: Data.Sensitive Prelude.Text,
    -- | The Amazon Resource Number (ARN) of the endpoint. For information about
    -- endpoints, see
    -- <https://docs.aws.amazon.com/comprehend/latest/dg/manage-endpoints.html Managing endpoints>.
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
-- 'endpointArn', 'classifyDocument_endpointArn' - The Amazon Resource Number (ARN) of the endpoint. For information about
-- endpoints, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/manage-endpoints.html Managing endpoints>.
newClassifyDocument ::
  -- | 'text'
  Prelude.Text ->
  -- | 'endpointArn'
  Prelude.Text ->
  ClassifyDocument
newClassifyDocument pText_ pEndpointArn_ =
  ClassifyDocument'
    { text =
        Data._Sensitive Lens.# pText_,
      endpointArn = pEndpointArn_
    }

-- | The document text to be analyzed.
classifyDocument_text :: Lens.Lens' ClassifyDocument Prelude.Text
classifyDocument_text = Lens.lens (\ClassifyDocument' {text} -> text) (\s@ClassifyDocument' {} a -> s {text = a} :: ClassifyDocument) Prelude.. Data._Sensitive

-- | The Amazon Resource Number (ARN) of the endpoint. For information about
-- endpoints, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/manage-endpoints.html Managing endpoints>.
classifyDocument_endpointArn :: Lens.Lens' ClassifyDocument Prelude.Text
classifyDocument_endpointArn = Lens.lens (\ClassifyDocument' {endpointArn} -> endpointArn) (\s@ClassifyDocument' {} a -> s {endpointArn = a} :: ClassifyDocument)

instance Core.AWSRequest ClassifyDocument where
  type
    AWSResponse ClassifyDocument =
      ClassifyDocumentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ClassifyDocumentResponse'
            Prelude.<$> (x Data..?> "Classes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Labels" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ClassifyDocument where
  hashWithSalt _salt ClassifyDocument' {..} =
    _salt `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` endpointArn

instance Prelude.NFData ClassifyDocument where
  rnf ClassifyDocument' {..} =
    Prelude.rnf text
      `Prelude.seq` Prelude.rnf endpointArn

instance Data.ToHeaders ClassifyDocument where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.ClassifyDocument" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ClassifyDocument where
  toJSON ClassifyDocument' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Text" Data..= text),
            Prelude.Just ("EndpointArn" Data..= endpointArn)
          ]
      )

instance Data.ToPath ClassifyDocument where
  toPath = Prelude.const "/"

instance Data.ToQuery ClassifyDocument where
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
classifyDocumentResponse_classes = Lens.lens (\ClassifyDocumentResponse' {classes} -> classes) (\s@ClassifyDocumentResponse' {} a -> s {classes = a} :: ClassifyDocumentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The labels used the document being analyzed. These are used for
-- multi-label trained models. Individual labels represent different
-- categories that are related in some manner and are not mutually
-- exclusive. For example, a movie can be just an action movie, or it can
-- be an action movie, a science fiction movie, and a comedy, all at the
-- same time.
classifyDocumentResponse_labels :: Lens.Lens' ClassifyDocumentResponse (Prelude.Maybe [DocumentLabel])
classifyDocumentResponse_labels = Lens.lens (\ClassifyDocumentResponse' {labels} -> labels) (\s@ClassifyDocumentResponse' {} a -> s {labels = a} :: ClassifyDocumentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
classifyDocumentResponse_httpStatus :: Lens.Lens' ClassifyDocumentResponse Prelude.Int
classifyDocumentResponse_httpStatus = Lens.lens (\ClassifyDocumentResponse' {httpStatus} -> httpStatus) (\s@ClassifyDocumentResponse' {} a -> s {httpStatus = a} :: ClassifyDocumentResponse)

instance Prelude.NFData ClassifyDocumentResponse where
  rnf ClassifyDocumentResponse' {..} =
    Prelude.rnf classes
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf httpStatus
