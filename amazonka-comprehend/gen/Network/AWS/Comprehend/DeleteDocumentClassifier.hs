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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDocumentClassifier' smart constructor.
data DeleteDocumentClassifier = DeleteDocumentClassifier'
  { -- | The Amazon Resource Name (ARN) that identifies the document classifier.
    documentClassifierArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteDocumentClassifier
newDeleteDocumentClassifier pDocumentClassifierArn_ =
  DeleteDocumentClassifier'
    { documentClassifierArn =
        pDocumentClassifierArn_
    }

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
deleteDocumentClassifier_documentClassifierArn :: Lens.Lens' DeleteDocumentClassifier Prelude.Text
deleteDocumentClassifier_documentClassifierArn = Lens.lens (\DeleteDocumentClassifier' {documentClassifierArn} -> documentClassifierArn) (\s@DeleteDocumentClassifier' {} a -> s {documentClassifierArn = a} :: DeleteDocumentClassifier)

instance Prelude.AWSRequest DeleteDocumentClassifier where
  type
    Rs DeleteDocumentClassifier =
      DeleteDocumentClassifierResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDocumentClassifierResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDocumentClassifier

instance Prelude.NFData DeleteDocumentClassifier

instance Prelude.ToHeaders DeleteDocumentClassifier where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Comprehend_20171127.DeleteDocumentClassifier" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteDocumentClassifier where
  toJSON DeleteDocumentClassifier' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "DocumentClassifierArn"
                  Prelude..= documentClassifierArn
              )
          ]
      )

instance Prelude.ToPath DeleteDocumentClassifier where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteDocumentClassifier where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDocumentClassifierResponse' smart constructor.
data DeleteDocumentClassifierResponse = DeleteDocumentClassifierResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteDocumentClassifierResponse
newDeleteDocumentClassifierResponse pHttpStatus_ =
  DeleteDocumentClassifierResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDocumentClassifierResponse_httpStatus :: Lens.Lens' DeleteDocumentClassifierResponse Prelude.Int
deleteDocumentClassifierResponse_httpStatus = Lens.lens (\DeleteDocumentClassifierResponse' {httpStatus} -> httpStatus) (\s@DeleteDocumentClassifierResponse' {} a -> s {httpStatus = a} :: DeleteDocumentClassifierResponse)

instance
  Prelude.NFData
    DeleteDocumentClassifierResponse
