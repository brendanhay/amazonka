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
-- Module      : Network.AWS.Glue.CreateClassifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a classifier in the user\'s account. This can be a
-- @GrokClassifier@, an @XMLClassifier@, a @JsonClassifier@, or a
-- @CsvClassifier@, depending on which field of the request is present.
module Network.AWS.Glue.CreateClassifier
  ( -- * Creating a Request
    CreateClassifier (..),
    newCreateClassifier,

    -- * Request Lenses
    createClassifier_xMLClassifier,
    createClassifier_jsonClassifier,
    createClassifier_csvClassifier,
    createClassifier_grokClassifier,

    -- * Destructuring the Response
    CreateClassifierResponse (..),
    newCreateClassifierResponse,

    -- * Response Lenses
    createClassifierResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateClassifier' smart constructor.
data CreateClassifier = CreateClassifier'
  { -- | An @XMLClassifier@ object specifying the classifier to create.
    xMLClassifier :: Core.Maybe CreateXMLClassifierRequest,
    -- | A @JsonClassifier@ object specifying the classifier to create.
    jsonClassifier :: Core.Maybe CreateJsonClassifierRequest,
    -- | A @CsvClassifier@ object specifying the classifier to create.
    csvClassifier :: Core.Maybe CreateCsvClassifierRequest,
    -- | A @GrokClassifier@ object specifying the classifier to create.
    grokClassifier :: Core.Maybe CreateGrokClassifierRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateClassifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'xMLClassifier', 'createClassifier_xMLClassifier' - An @XMLClassifier@ object specifying the classifier to create.
--
-- 'jsonClassifier', 'createClassifier_jsonClassifier' - A @JsonClassifier@ object specifying the classifier to create.
--
-- 'csvClassifier', 'createClassifier_csvClassifier' - A @CsvClassifier@ object specifying the classifier to create.
--
-- 'grokClassifier', 'createClassifier_grokClassifier' - A @GrokClassifier@ object specifying the classifier to create.
newCreateClassifier ::
  CreateClassifier
newCreateClassifier =
  CreateClassifier'
    { xMLClassifier = Core.Nothing,
      jsonClassifier = Core.Nothing,
      csvClassifier = Core.Nothing,
      grokClassifier = Core.Nothing
    }

-- | An @XMLClassifier@ object specifying the classifier to create.
createClassifier_xMLClassifier :: Lens.Lens' CreateClassifier (Core.Maybe CreateXMLClassifierRequest)
createClassifier_xMLClassifier = Lens.lens (\CreateClassifier' {xMLClassifier} -> xMLClassifier) (\s@CreateClassifier' {} a -> s {xMLClassifier = a} :: CreateClassifier)

-- | A @JsonClassifier@ object specifying the classifier to create.
createClassifier_jsonClassifier :: Lens.Lens' CreateClassifier (Core.Maybe CreateJsonClassifierRequest)
createClassifier_jsonClassifier = Lens.lens (\CreateClassifier' {jsonClassifier} -> jsonClassifier) (\s@CreateClassifier' {} a -> s {jsonClassifier = a} :: CreateClassifier)

-- | A @CsvClassifier@ object specifying the classifier to create.
createClassifier_csvClassifier :: Lens.Lens' CreateClassifier (Core.Maybe CreateCsvClassifierRequest)
createClassifier_csvClassifier = Lens.lens (\CreateClassifier' {csvClassifier} -> csvClassifier) (\s@CreateClassifier' {} a -> s {csvClassifier = a} :: CreateClassifier)

-- | A @GrokClassifier@ object specifying the classifier to create.
createClassifier_grokClassifier :: Lens.Lens' CreateClassifier (Core.Maybe CreateGrokClassifierRequest)
createClassifier_grokClassifier = Lens.lens (\CreateClassifier' {grokClassifier} -> grokClassifier) (\s@CreateClassifier' {} a -> s {grokClassifier = a} :: CreateClassifier)

instance Core.AWSRequest CreateClassifier where
  type
    AWSResponse CreateClassifier =
      CreateClassifierResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateClassifierResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateClassifier

instance Core.NFData CreateClassifier

instance Core.ToHeaders CreateClassifier where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CreateClassifier" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateClassifier where
  toJSON CreateClassifier' {..} =
    Core.object
      ( Core.catMaybes
          [ ("XMLClassifier" Core..=) Core.<$> xMLClassifier,
            ("JsonClassifier" Core..=) Core.<$> jsonClassifier,
            ("CsvClassifier" Core..=) Core.<$> csvClassifier,
            ("GrokClassifier" Core..=) Core.<$> grokClassifier
          ]
      )

instance Core.ToPath CreateClassifier where
  toPath = Core.const "/"

instance Core.ToQuery CreateClassifier where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateClassifierResponse' smart constructor.
data CreateClassifierResponse = CreateClassifierResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateClassifierResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createClassifierResponse_httpStatus' - The response's http status code.
newCreateClassifierResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateClassifierResponse
newCreateClassifierResponse pHttpStatus_ =
  CreateClassifierResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createClassifierResponse_httpStatus :: Lens.Lens' CreateClassifierResponse Core.Int
createClassifierResponse_httpStatus = Lens.lens (\CreateClassifierResponse' {httpStatus} -> httpStatus) (\s@CreateClassifierResponse' {} a -> s {httpStatus = a} :: CreateClassifierResponse)

instance Core.NFData CreateClassifierResponse
