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
-- Module      : Network.AWS.Glue.GetClassifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a classifier by name.
module Network.AWS.Glue.GetClassifier
  ( -- * Creating a Request
    GetClassifier (..),
    newGetClassifier,

    -- * Request Lenses
    getClassifier_name,

    -- * Destructuring the Response
    GetClassifierResponse (..),
    newGetClassifierResponse,

    -- * Response Lenses
    getClassifierResponse_classifier,
    getClassifierResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetClassifier' smart constructor.
data GetClassifier = GetClassifier'
  { -- | Name of the classifier to retrieve.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetClassifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getClassifier_name' - Name of the classifier to retrieve.
newGetClassifier ::
  -- | 'name'
  Core.Text ->
  GetClassifier
newGetClassifier pName_ =
  GetClassifier' {name = pName_}

-- | Name of the classifier to retrieve.
getClassifier_name :: Lens.Lens' GetClassifier Core.Text
getClassifier_name = Lens.lens (\GetClassifier' {name} -> name) (\s@GetClassifier' {} a -> s {name = a} :: GetClassifier)

instance Core.AWSRequest GetClassifier where
  type
    AWSResponse GetClassifier =
      GetClassifierResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetClassifierResponse'
            Core.<$> (x Core..?> "Classifier")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetClassifier

instance Core.NFData GetClassifier

instance Core.ToHeaders GetClassifier where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetClassifier" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetClassifier where
  toJSON GetClassifier' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath GetClassifier where
  toPath = Core.const "/"

instance Core.ToQuery GetClassifier where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetClassifierResponse' smart constructor.
data GetClassifierResponse = GetClassifierResponse'
  { -- | The requested classifier.
    classifier :: Core.Maybe Classifier,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetClassifierResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'classifier', 'getClassifierResponse_classifier' - The requested classifier.
--
-- 'httpStatus', 'getClassifierResponse_httpStatus' - The response's http status code.
newGetClassifierResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetClassifierResponse
newGetClassifierResponse pHttpStatus_ =
  GetClassifierResponse'
    { classifier = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested classifier.
getClassifierResponse_classifier :: Lens.Lens' GetClassifierResponse (Core.Maybe Classifier)
getClassifierResponse_classifier = Lens.lens (\GetClassifierResponse' {classifier} -> classifier) (\s@GetClassifierResponse' {} a -> s {classifier = a} :: GetClassifierResponse)

-- | The response's http status code.
getClassifierResponse_httpStatus :: Lens.Lens' GetClassifierResponse Core.Int
getClassifierResponse_httpStatus = Lens.lens (\GetClassifierResponse' {httpStatus} -> httpStatus) (\s@GetClassifierResponse' {} a -> s {httpStatus = a} :: GetClassifierResponse)

instance Core.NFData GetClassifierResponse
