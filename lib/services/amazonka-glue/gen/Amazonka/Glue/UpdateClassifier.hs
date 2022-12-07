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
-- Module      : Amazonka.Glue.UpdateClassifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing classifier (a @GrokClassifier@, an @XMLClassifier@,
-- a @JsonClassifier@, or a @CsvClassifier@, depending on which field is
-- present).
module Amazonka.Glue.UpdateClassifier
  ( -- * Creating a Request
    UpdateClassifier (..),
    newUpdateClassifier,

    -- * Request Lenses
    updateClassifier_csvClassifier,
    updateClassifier_xMLClassifier,
    updateClassifier_grokClassifier,
    updateClassifier_jsonClassifier,

    -- * Destructuring the Response
    UpdateClassifierResponse (..),
    newUpdateClassifierResponse,

    -- * Response Lenses
    updateClassifierResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateClassifier' smart constructor.
data UpdateClassifier = UpdateClassifier'
  { -- | A @CsvClassifier@ object with updated fields.
    csvClassifier :: Prelude.Maybe UpdateCsvClassifierRequest,
    -- | An @XMLClassifier@ object with updated fields.
    xMLClassifier :: Prelude.Maybe UpdateXMLClassifierRequest,
    -- | A @GrokClassifier@ object with updated fields.
    grokClassifier :: Prelude.Maybe UpdateGrokClassifierRequest,
    -- | A @JsonClassifier@ object with updated fields.
    jsonClassifier :: Prelude.Maybe UpdateJsonClassifierRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClassifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'csvClassifier', 'updateClassifier_csvClassifier' - A @CsvClassifier@ object with updated fields.
--
-- 'xMLClassifier', 'updateClassifier_xMLClassifier' - An @XMLClassifier@ object with updated fields.
--
-- 'grokClassifier', 'updateClassifier_grokClassifier' - A @GrokClassifier@ object with updated fields.
--
-- 'jsonClassifier', 'updateClassifier_jsonClassifier' - A @JsonClassifier@ object with updated fields.
newUpdateClassifier ::
  UpdateClassifier
newUpdateClassifier =
  UpdateClassifier'
    { csvClassifier = Prelude.Nothing,
      xMLClassifier = Prelude.Nothing,
      grokClassifier = Prelude.Nothing,
      jsonClassifier = Prelude.Nothing
    }

-- | A @CsvClassifier@ object with updated fields.
updateClassifier_csvClassifier :: Lens.Lens' UpdateClassifier (Prelude.Maybe UpdateCsvClassifierRequest)
updateClassifier_csvClassifier = Lens.lens (\UpdateClassifier' {csvClassifier} -> csvClassifier) (\s@UpdateClassifier' {} a -> s {csvClassifier = a} :: UpdateClassifier)

-- | An @XMLClassifier@ object with updated fields.
updateClassifier_xMLClassifier :: Lens.Lens' UpdateClassifier (Prelude.Maybe UpdateXMLClassifierRequest)
updateClassifier_xMLClassifier = Lens.lens (\UpdateClassifier' {xMLClassifier} -> xMLClassifier) (\s@UpdateClassifier' {} a -> s {xMLClassifier = a} :: UpdateClassifier)

-- | A @GrokClassifier@ object with updated fields.
updateClassifier_grokClassifier :: Lens.Lens' UpdateClassifier (Prelude.Maybe UpdateGrokClassifierRequest)
updateClassifier_grokClassifier = Lens.lens (\UpdateClassifier' {grokClassifier} -> grokClassifier) (\s@UpdateClassifier' {} a -> s {grokClassifier = a} :: UpdateClassifier)

-- | A @JsonClassifier@ object with updated fields.
updateClassifier_jsonClassifier :: Lens.Lens' UpdateClassifier (Prelude.Maybe UpdateJsonClassifierRequest)
updateClassifier_jsonClassifier = Lens.lens (\UpdateClassifier' {jsonClassifier} -> jsonClassifier) (\s@UpdateClassifier' {} a -> s {jsonClassifier = a} :: UpdateClassifier)

instance Core.AWSRequest UpdateClassifier where
  type
    AWSResponse UpdateClassifier =
      UpdateClassifierResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateClassifierResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateClassifier where
  hashWithSalt _salt UpdateClassifier' {..} =
    _salt `Prelude.hashWithSalt` csvClassifier
      `Prelude.hashWithSalt` xMLClassifier
      `Prelude.hashWithSalt` grokClassifier
      `Prelude.hashWithSalt` jsonClassifier

instance Prelude.NFData UpdateClassifier where
  rnf UpdateClassifier' {..} =
    Prelude.rnf csvClassifier
      `Prelude.seq` Prelude.rnf xMLClassifier
      `Prelude.seq` Prelude.rnf grokClassifier
      `Prelude.seq` Prelude.rnf jsonClassifier

instance Data.ToHeaders UpdateClassifier where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.UpdateClassifier" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateClassifier where
  toJSON UpdateClassifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CsvClassifier" Data..=) Prelude.<$> csvClassifier,
            ("XMLClassifier" Data..=) Prelude.<$> xMLClassifier,
            ("GrokClassifier" Data..=)
              Prelude.<$> grokClassifier,
            ("JsonClassifier" Data..=)
              Prelude.<$> jsonClassifier
          ]
      )

instance Data.ToPath UpdateClassifier where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateClassifier where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateClassifierResponse' smart constructor.
data UpdateClassifierResponse = UpdateClassifierResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClassifierResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateClassifierResponse_httpStatus' - The response's http status code.
newUpdateClassifierResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateClassifierResponse
newUpdateClassifierResponse pHttpStatus_ =
  UpdateClassifierResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateClassifierResponse_httpStatus :: Lens.Lens' UpdateClassifierResponse Prelude.Int
updateClassifierResponse_httpStatus = Lens.lens (\UpdateClassifierResponse' {httpStatus} -> httpStatus) (\s@UpdateClassifierResponse' {} a -> s {httpStatus = a} :: UpdateClassifierResponse)

instance Prelude.NFData UpdateClassifierResponse where
  rnf UpdateClassifierResponse' {..} =
    Prelude.rnf httpStatus
