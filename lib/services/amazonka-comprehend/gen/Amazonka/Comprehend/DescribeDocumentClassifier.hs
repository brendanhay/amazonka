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
-- Module      : Amazonka.Comprehend.DescribeDocumentClassifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a document classifier.
module Amazonka.Comprehend.DescribeDocumentClassifier
  ( -- * Creating a Request
    DescribeDocumentClassifier (..),
    newDescribeDocumentClassifier,

    -- * Request Lenses
    describeDocumentClassifier_documentClassifierArn,

    -- * Destructuring the Response
    DescribeDocumentClassifierResponse (..),
    newDescribeDocumentClassifierResponse,

    -- * Response Lenses
    describeDocumentClassifierResponse_documentClassifierProperties,
    describeDocumentClassifierResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDocumentClassifier' smart constructor.
data DescribeDocumentClassifier = DescribeDocumentClassifier'
  { -- | The Amazon Resource Name (ARN) that identifies the document classifier.
    -- The operation returns this identifier in its response.
    documentClassifierArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDocumentClassifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentClassifierArn', 'describeDocumentClassifier_documentClassifierArn' - The Amazon Resource Name (ARN) that identifies the document classifier.
-- The operation returns this identifier in its response.
newDescribeDocumentClassifier ::
  -- | 'documentClassifierArn'
  Prelude.Text ->
  DescribeDocumentClassifier
newDescribeDocumentClassifier pDocumentClassifierArn_ =
  DescribeDocumentClassifier'
    { documentClassifierArn =
        pDocumentClassifierArn_
    }

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
-- The operation returns this identifier in its response.
describeDocumentClassifier_documentClassifierArn :: Lens.Lens' DescribeDocumentClassifier Prelude.Text
describeDocumentClassifier_documentClassifierArn = Lens.lens (\DescribeDocumentClassifier' {documentClassifierArn} -> documentClassifierArn) (\s@DescribeDocumentClassifier' {} a -> s {documentClassifierArn = a} :: DescribeDocumentClassifier)

instance Core.AWSRequest DescribeDocumentClassifier where
  type
    AWSResponse DescribeDocumentClassifier =
      DescribeDocumentClassifierResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDocumentClassifierResponse'
            Prelude.<$> (x Data..?> "DocumentClassifierProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDocumentClassifier where
  hashWithSalt _salt DescribeDocumentClassifier' {..} =
    _salt `Prelude.hashWithSalt` documentClassifierArn

instance Prelude.NFData DescribeDocumentClassifier where
  rnf DescribeDocumentClassifier' {..} =
    Prelude.rnf documentClassifierArn

instance Data.ToHeaders DescribeDocumentClassifier where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.DescribeDocumentClassifier" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDocumentClassifier where
  toJSON DescribeDocumentClassifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "DocumentClassifierArn"
                  Data..= documentClassifierArn
              )
          ]
      )

instance Data.ToPath DescribeDocumentClassifier where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDocumentClassifier where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDocumentClassifierResponse' smart constructor.
data DescribeDocumentClassifierResponse = DescribeDocumentClassifierResponse'
  { -- | An object that contains the properties associated with a document
    -- classifier.
    documentClassifierProperties :: Prelude.Maybe DocumentClassifierProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDocumentClassifierResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentClassifierProperties', 'describeDocumentClassifierResponse_documentClassifierProperties' - An object that contains the properties associated with a document
-- classifier.
--
-- 'httpStatus', 'describeDocumentClassifierResponse_httpStatus' - The response's http status code.
newDescribeDocumentClassifierResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDocumentClassifierResponse
newDescribeDocumentClassifierResponse pHttpStatus_ =
  DescribeDocumentClassifierResponse'
    { documentClassifierProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains the properties associated with a document
-- classifier.
describeDocumentClassifierResponse_documentClassifierProperties :: Lens.Lens' DescribeDocumentClassifierResponse (Prelude.Maybe DocumentClassifierProperties)
describeDocumentClassifierResponse_documentClassifierProperties = Lens.lens (\DescribeDocumentClassifierResponse' {documentClassifierProperties} -> documentClassifierProperties) (\s@DescribeDocumentClassifierResponse' {} a -> s {documentClassifierProperties = a} :: DescribeDocumentClassifierResponse)

-- | The response's http status code.
describeDocumentClassifierResponse_httpStatus :: Lens.Lens' DescribeDocumentClassifierResponse Prelude.Int
describeDocumentClassifierResponse_httpStatus = Lens.lens (\DescribeDocumentClassifierResponse' {httpStatus} -> httpStatus) (\s@DescribeDocumentClassifierResponse' {} a -> s {httpStatus = a} :: DescribeDocumentClassifierResponse)

instance
  Prelude.NFData
    DescribeDocumentClassifierResponse
  where
  rnf DescribeDocumentClassifierResponse' {..} =
    Prelude.rnf documentClassifierProperties `Prelude.seq`
      Prelude.rnf httpStatus
