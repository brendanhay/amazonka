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
-- Module      : Network.AWS.Comprehend.DeleteDocumentClassifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously created document classifier
--
-- Only those classifiers that are in terminated states (IN_ERROR, TRAINED)
-- will be deleted. If an active inference job is using the model, a
-- @ResourceInUseException@ will be returned.
--
-- This is an asynchronous action that puts the classifier into a DELETING
-- state, and it is then removed by a background job. Once removed, the
-- classifier disappears from your account and is no longer available for
-- use.
module Network.AWS.Comprehend.DeleteDocumentClassifier
  ( -- * Creating a Request
    DeleteDocumentClassifier (..),
    newDeleteDocumentClassifier,

    -- * Request Lenses
    deleteDocumentClassifier_documentClassifierArn,

    -- * Destructuring the Response
    DeleteDocumentClassifierResponse (..),
    newDeleteDocumentClassifierResponse,

    -- * Response Lenses
    deleteDocumentClassifierResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDocumentClassifier' smart constructor.
data DeleteDocumentClassifier = DeleteDocumentClassifier'
  { -- | The Amazon Resource Name (ARN) that identifies the document classifier.
    documentClassifierArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDocumentClassifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentClassifierArn', 'deleteDocumentClassifier_documentClassifierArn' - The Amazon Resource Name (ARN) that identifies the document classifier.
newDeleteDocumentClassifier ::
  -- | 'documentClassifierArn'
  Core.Text ->
  DeleteDocumentClassifier
newDeleteDocumentClassifier pDocumentClassifierArn_ =
  DeleteDocumentClassifier'
    { documentClassifierArn =
        pDocumentClassifierArn_
    }

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
deleteDocumentClassifier_documentClassifierArn :: Lens.Lens' DeleteDocumentClassifier Core.Text
deleteDocumentClassifier_documentClassifierArn = Lens.lens (\DeleteDocumentClassifier' {documentClassifierArn} -> documentClassifierArn) (\s@DeleteDocumentClassifier' {} a -> s {documentClassifierArn = a} :: DeleteDocumentClassifier)

instance Core.AWSRequest DeleteDocumentClassifier where
  type
    AWSResponse DeleteDocumentClassifier =
      DeleteDocumentClassifierResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDocumentClassifierResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteDocumentClassifier

instance Core.NFData DeleteDocumentClassifier

instance Core.ToHeaders DeleteDocumentClassifier where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DeleteDocumentClassifier" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteDocumentClassifier where
  toJSON DeleteDocumentClassifier' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "DocumentClassifierArn"
                  Core..= documentClassifierArn
              )
          ]
      )

instance Core.ToPath DeleteDocumentClassifier where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDocumentClassifier where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteDocumentClassifierResponse' smart constructor.
data DeleteDocumentClassifierResponse = DeleteDocumentClassifierResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDocumentClassifierResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDocumentClassifierResponse_httpStatus' - The response's http status code.
newDeleteDocumentClassifierResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteDocumentClassifierResponse
newDeleteDocumentClassifierResponse pHttpStatus_ =
  DeleteDocumentClassifierResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDocumentClassifierResponse_httpStatus :: Lens.Lens' DeleteDocumentClassifierResponse Core.Int
deleteDocumentClassifierResponse_httpStatus = Lens.lens (\DeleteDocumentClassifierResponse' {httpStatus} -> httpStatus) (\s@DeleteDocumentClassifierResponse' {} a -> s {httpStatus = a} :: DeleteDocumentClassifierResponse)

instance Core.NFData DeleteDocumentClassifierResponse
