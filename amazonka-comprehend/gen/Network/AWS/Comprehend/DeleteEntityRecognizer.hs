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
-- Module      : Network.AWS.Comprehend.DeleteEntityRecognizer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an entity recognizer.
--
-- Only those recognizers that are in terminated states (IN_ERROR, TRAINED)
-- will be deleted. If an active inference job is using the model, a
-- @ResourceInUseException@ will be returned.
--
-- This is an asynchronous action that puts the recognizer into a DELETING
-- state, and it is then removed by a background job. Once removed, the
-- recognizer disappears from your account and is no longer available for
-- use.
module Network.AWS.Comprehend.DeleteEntityRecognizer
  ( -- * Creating a Request
    DeleteEntityRecognizer (..),
    newDeleteEntityRecognizer,

    -- * Request Lenses
    deleteEntityRecognizer_entityRecognizerArn,

    -- * Destructuring the Response
    DeleteEntityRecognizerResponse (..),
    newDeleteEntityRecognizerResponse,

    -- * Response Lenses
    deleteEntityRecognizerResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteEntityRecognizer' smart constructor.
data DeleteEntityRecognizer = DeleteEntityRecognizer'
  { -- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
    entityRecognizerArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteEntityRecognizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityRecognizerArn', 'deleteEntityRecognizer_entityRecognizerArn' - The Amazon Resource Name (ARN) that identifies the entity recognizer.
newDeleteEntityRecognizer ::
  -- | 'entityRecognizerArn'
  Core.Text ->
  DeleteEntityRecognizer
newDeleteEntityRecognizer pEntityRecognizerArn_ =
  DeleteEntityRecognizer'
    { entityRecognizerArn =
        pEntityRecognizerArn_
    }

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
deleteEntityRecognizer_entityRecognizerArn :: Lens.Lens' DeleteEntityRecognizer Core.Text
deleteEntityRecognizer_entityRecognizerArn = Lens.lens (\DeleteEntityRecognizer' {entityRecognizerArn} -> entityRecognizerArn) (\s@DeleteEntityRecognizer' {} a -> s {entityRecognizerArn = a} :: DeleteEntityRecognizer)

instance Core.AWSRequest DeleteEntityRecognizer where
  type
    AWSResponse DeleteEntityRecognizer =
      DeleteEntityRecognizerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEntityRecognizerResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteEntityRecognizer

instance Core.NFData DeleteEntityRecognizer

instance Core.ToHeaders DeleteEntityRecognizer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DeleteEntityRecognizer" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteEntityRecognizer where
  toJSON DeleteEntityRecognizer' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("EntityRecognizerArn" Core..= entityRecognizerArn)
          ]
      )

instance Core.ToPath DeleteEntityRecognizer where
  toPath = Core.const "/"

instance Core.ToQuery DeleteEntityRecognizer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteEntityRecognizerResponse' smart constructor.
data DeleteEntityRecognizerResponse = DeleteEntityRecognizerResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteEntityRecognizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEntityRecognizerResponse_httpStatus' - The response's http status code.
newDeleteEntityRecognizerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteEntityRecognizerResponse
newDeleteEntityRecognizerResponse pHttpStatus_ =
  DeleteEntityRecognizerResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteEntityRecognizerResponse_httpStatus :: Lens.Lens' DeleteEntityRecognizerResponse Core.Int
deleteEntityRecognizerResponse_httpStatus = Lens.lens (\DeleteEntityRecognizerResponse' {httpStatus} -> httpStatus) (\s@DeleteEntityRecognizerResponse' {} a -> s {httpStatus = a} :: DeleteEntityRecognizerResponse)

instance Core.NFData DeleteEntityRecognizerResponse
