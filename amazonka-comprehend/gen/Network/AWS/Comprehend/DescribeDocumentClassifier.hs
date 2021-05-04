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
-- Module      : Network.AWS.Comprehend.DescribeDocumentClassifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a document classifier.
module Network.AWS.Comprehend.DescribeDocumentClassifier
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

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDocumentClassifier' smart constructor.
data DescribeDocumentClassifier = DescribeDocumentClassifier'
  { -- | The Amazon Resource Name (ARN) that identifies the document classifier.
    -- The operation returns this identifier in its response.
    documentClassifierArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.AWSRequest
    DescribeDocumentClassifier
  where
  type
    Rs DescribeDocumentClassifier =
      DescribeDocumentClassifierResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDocumentClassifierResponse'
            Prelude.<$> (x Prelude..?> "DocumentClassifierProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDocumentClassifier

instance Prelude.NFData DescribeDocumentClassifier

instance Prelude.ToHeaders DescribeDocumentClassifier where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Comprehend_20171127.DescribeDocumentClassifier" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeDocumentClassifier where
  toJSON DescribeDocumentClassifier' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "DocumentClassifierArn"
                  Prelude..= documentClassifierArn
              )
          ]
      )

instance Prelude.ToPath DescribeDocumentClassifier where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeDocumentClassifier where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDocumentClassifierResponse' smart constructor.
data DescribeDocumentClassifierResponse = DescribeDocumentClassifierResponse'
  { -- | An object that contains the properties associated with a document
    -- classifier.
    documentClassifierProperties :: Prelude.Maybe DocumentClassifierProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
