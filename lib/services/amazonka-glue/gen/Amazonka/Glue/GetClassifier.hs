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
-- Module      : Amazonka.Glue.GetClassifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a classifier by name.
module Amazonka.Glue.GetClassifier
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetClassifier' smart constructor.
data GetClassifier = GetClassifier'
  { -- | Name of the classifier to retrieve.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetClassifier
newGetClassifier pName_ =
  GetClassifier' {name = pName_}

-- | Name of the classifier to retrieve.
getClassifier_name :: Lens.Lens' GetClassifier Prelude.Text
getClassifier_name = Lens.lens (\GetClassifier' {name} -> name) (\s@GetClassifier' {} a -> s {name = a} :: GetClassifier)

instance Core.AWSRequest GetClassifier where
  type
    AWSResponse GetClassifier =
      GetClassifierResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetClassifierResponse'
            Prelude.<$> (x Data..?> "Classifier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetClassifier where
  hashWithSalt _salt GetClassifier' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetClassifier where
  rnf GetClassifier' {..} = Prelude.rnf name

instance Data.ToHeaders GetClassifier where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetClassifier" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetClassifier where
  toJSON GetClassifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath GetClassifier where
  toPath = Prelude.const "/"

instance Data.ToQuery GetClassifier where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetClassifierResponse' smart constructor.
data GetClassifierResponse = GetClassifierResponse'
  { -- | The requested classifier.
    classifier :: Prelude.Maybe Classifier,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetClassifierResponse
newGetClassifierResponse pHttpStatus_ =
  GetClassifierResponse'
    { classifier =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested classifier.
getClassifierResponse_classifier :: Lens.Lens' GetClassifierResponse (Prelude.Maybe Classifier)
getClassifierResponse_classifier = Lens.lens (\GetClassifierResponse' {classifier} -> classifier) (\s@GetClassifierResponse' {} a -> s {classifier = a} :: GetClassifierResponse)

-- | The response's http status code.
getClassifierResponse_httpStatus :: Lens.Lens' GetClassifierResponse Prelude.Int
getClassifierResponse_httpStatus = Lens.lens (\GetClassifierResponse' {httpStatus} -> httpStatus) (\s@GetClassifierResponse' {} a -> s {httpStatus = a} :: GetClassifierResponse)

instance Prelude.NFData GetClassifierResponse where
  rnf GetClassifierResponse' {..} =
    Prelude.rnf classifier
      `Prelude.seq` Prelude.rnf httpStatus
