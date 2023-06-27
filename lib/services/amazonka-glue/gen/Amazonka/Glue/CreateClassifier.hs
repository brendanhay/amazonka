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
-- Module      : Amazonka.Glue.CreateClassifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a classifier in the user\'s account. This can be a
-- @GrokClassifier@, an @XMLClassifier@, a @JsonClassifier@, or a
-- @CsvClassifier@, depending on which field of the request is present.
module Amazonka.Glue.CreateClassifier
  ( -- * Creating a Request
    CreateClassifier (..),
    newCreateClassifier,

    -- * Request Lenses
    createClassifier_csvClassifier,
    createClassifier_grokClassifier,
    createClassifier_jsonClassifier,
    createClassifier_xMLClassifier,

    -- * Destructuring the Response
    CreateClassifierResponse (..),
    newCreateClassifierResponse,

    -- * Response Lenses
    createClassifierResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateClassifier' smart constructor.
data CreateClassifier = CreateClassifier'
  { -- | A @CsvClassifier@ object specifying the classifier to create.
    csvClassifier :: Prelude.Maybe CreateCsvClassifierRequest,
    -- | A @GrokClassifier@ object specifying the classifier to create.
    grokClassifier :: Prelude.Maybe CreateGrokClassifierRequest,
    -- | A @JsonClassifier@ object specifying the classifier to create.
    jsonClassifier :: Prelude.Maybe CreateJsonClassifierRequest,
    -- | An @XMLClassifier@ object specifying the classifier to create.
    xMLClassifier :: Prelude.Maybe CreateXMLClassifierRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateClassifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'csvClassifier', 'createClassifier_csvClassifier' - A @CsvClassifier@ object specifying the classifier to create.
--
-- 'grokClassifier', 'createClassifier_grokClassifier' - A @GrokClassifier@ object specifying the classifier to create.
--
-- 'jsonClassifier', 'createClassifier_jsonClassifier' - A @JsonClassifier@ object specifying the classifier to create.
--
-- 'xMLClassifier', 'createClassifier_xMLClassifier' - An @XMLClassifier@ object specifying the classifier to create.
newCreateClassifier ::
  CreateClassifier
newCreateClassifier =
  CreateClassifier'
    { csvClassifier = Prelude.Nothing,
      grokClassifier = Prelude.Nothing,
      jsonClassifier = Prelude.Nothing,
      xMLClassifier = Prelude.Nothing
    }

-- | A @CsvClassifier@ object specifying the classifier to create.
createClassifier_csvClassifier :: Lens.Lens' CreateClassifier (Prelude.Maybe CreateCsvClassifierRequest)
createClassifier_csvClassifier = Lens.lens (\CreateClassifier' {csvClassifier} -> csvClassifier) (\s@CreateClassifier' {} a -> s {csvClassifier = a} :: CreateClassifier)

-- | A @GrokClassifier@ object specifying the classifier to create.
createClassifier_grokClassifier :: Lens.Lens' CreateClassifier (Prelude.Maybe CreateGrokClassifierRequest)
createClassifier_grokClassifier = Lens.lens (\CreateClassifier' {grokClassifier} -> grokClassifier) (\s@CreateClassifier' {} a -> s {grokClassifier = a} :: CreateClassifier)

-- | A @JsonClassifier@ object specifying the classifier to create.
createClassifier_jsonClassifier :: Lens.Lens' CreateClassifier (Prelude.Maybe CreateJsonClassifierRequest)
createClassifier_jsonClassifier = Lens.lens (\CreateClassifier' {jsonClassifier} -> jsonClassifier) (\s@CreateClassifier' {} a -> s {jsonClassifier = a} :: CreateClassifier)

-- | An @XMLClassifier@ object specifying the classifier to create.
createClassifier_xMLClassifier :: Lens.Lens' CreateClassifier (Prelude.Maybe CreateXMLClassifierRequest)
createClassifier_xMLClassifier = Lens.lens (\CreateClassifier' {xMLClassifier} -> xMLClassifier) (\s@CreateClassifier' {} a -> s {xMLClassifier = a} :: CreateClassifier)

instance Core.AWSRequest CreateClassifier where
  type
    AWSResponse CreateClassifier =
      CreateClassifierResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateClassifierResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateClassifier where
  hashWithSalt _salt CreateClassifier' {..} =
    _salt
      `Prelude.hashWithSalt` csvClassifier
      `Prelude.hashWithSalt` grokClassifier
      `Prelude.hashWithSalt` jsonClassifier
      `Prelude.hashWithSalt` xMLClassifier

instance Prelude.NFData CreateClassifier where
  rnf CreateClassifier' {..} =
    Prelude.rnf csvClassifier
      `Prelude.seq` Prelude.rnf grokClassifier
      `Prelude.seq` Prelude.rnf jsonClassifier
      `Prelude.seq` Prelude.rnf xMLClassifier

instance Data.ToHeaders CreateClassifier where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.CreateClassifier" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateClassifier where
  toJSON CreateClassifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CsvClassifier" Data..=) Prelude.<$> csvClassifier,
            ("GrokClassifier" Data..=)
              Prelude.<$> grokClassifier,
            ("JsonClassifier" Data..=)
              Prelude.<$> jsonClassifier,
            ("XMLClassifier" Data..=) Prelude.<$> xMLClassifier
          ]
      )

instance Data.ToPath CreateClassifier where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateClassifier where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateClassifierResponse' smart constructor.
data CreateClassifierResponse = CreateClassifierResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateClassifierResponse
newCreateClassifierResponse pHttpStatus_ =
  CreateClassifierResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createClassifierResponse_httpStatus :: Lens.Lens' CreateClassifierResponse Prelude.Int
createClassifierResponse_httpStatus = Lens.lens (\CreateClassifierResponse' {httpStatus} -> httpStatus) (\s@CreateClassifierResponse' {} a -> s {httpStatus = a} :: CreateClassifierResponse)

instance Prelude.NFData CreateClassifierResponse where
  rnf CreateClassifierResponse' {..} =
    Prelude.rnf httpStatus
