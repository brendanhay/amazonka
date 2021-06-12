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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDocumentClassifier' smart constructor.
data DescribeDocumentClassifier = DescribeDocumentClassifier'
  { -- | The Amazon Resource Name (ARN) that identifies the document classifier.
    -- The operation returns this identifier in its response.
    documentClassifierArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeDocumentClassifier
newDescribeDocumentClassifier pDocumentClassifierArn_ =
  DescribeDocumentClassifier'
    { documentClassifierArn =
        pDocumentClassifierArn_
    }

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
-- The operation returns this identifier in its response.
describeDocumentClassifier_documentClassifierArn :: Lens.Lens' DescribeDocumentClassifier Core.Text
describeDocumentClassifier_documentClassifierArn = Lens.lens (\DescribeDocumentClassifier' {documentClassifierArn} -> documentClassifierArn) (\s@DescribeDocumentClassifier' {} a -> s {documentClassifierArn = a} :: DescribeDocumentClassifier)

instance Core.AWSRequest DescribeDocumentClassifier where
  type
    AWSResponse DescribeDocumentClassifier =
      DescribeDocumentClassifierResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDocumentClassifierResponse'
            Core.<$> (x Core..?> "DocumentClassifierProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDocumentClassifier

instance Core.NFData DescribeDocumentClassifier

instance Core.ToHeaders DescribeDocumentClassifier where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DescribeDocumentClassifier" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeDocumentClassifier where
  toJSON DescribeDocumentClassifier' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "DocumentClassifierArn"
                  Core..= documentClassifierArn
              )
          ]
      )

instance Core.ToPath DescribeDocumentClassifier where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDocumentClassifier where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeDocumentClassifierResponse' smart constructor.
data DescribeDocumentClassifierResponse = DescribeDocumentClassifierResponse'
  { -- | An object that contains the properties associated with a document
    -- classifier.
    documentClassifierProperties :: Core.Maybe DocumentClassifierProperties,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeDocumentClassifierResponse
newDescribeDocumentClassifierResponse pHttpStatus_ =
  DescribeDocumentClassifierResponse'
    { documentClassifierProperties =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains the properties associated with a document
-- classifier.
describeDocumentClassifierResponse_documentClassifierProperties :: Lens.Lens' DescribeDocumentClassifierResponse (Core.Maybe DocumentClassifierProperties)
describeDocumentClassifierResponse_documentClassifierProperties = Lens.lens (\DescribeDocumentClassifierResponse' {documentClassifierProperties} -> documentClassifierProperties) (\s@DescribeDocumentClassifierResponse' {} a -> s {documentClassifierProperties = a} :: DescribeDocumentClassifierResponse)

-- | The response's http status code.
describeDocumentClassifierResponse_httpStatus :: Lens.Lens' DescribeDocumentClassifierResponse Core.Int
describeDocumentClassifierResponse_httpStatus = Lens.lens (\DescribeDocumentClassifierResponse' {httpStatus} -> httpStatus) (\s@DescribeDocumentClassifierResponse' {} a -> s {httpStatus = a} :: DescribeDocumentClassifierResponse)

instance
  Core.NFData
    DescribeDocumentClassifierResponse
