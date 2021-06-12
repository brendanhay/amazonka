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
-- Module      : Network.AWS.Comprehend.DescribeEntityRecognizer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about an entity recognizer including status, S3 buckets
-- containing training data, recognizer metadata, metrics, and so on.
module Network.AWS.Comprehend.DescribeEntityRecognizer
  ( -- * Creating a Request
    DescribeEntityRecognizer (..),
    newDescribeEntityRecognizer,

    -- * Request Lenses
    describeEntityRecognizer_entityRecognizerArn,

    -- * Destructuring the Response
    DescribeEntityRecognizerResponse (..),
    newDescribeEntityRecognizerResponse,

    -- * Response Lenses
    describeEntityRecognizerResponse_entityRecognizerProperties,
    describeEntityRecognizerResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEntityRecognizer' smart constructor.
data DescribeEntityRecognizer = DescribeEntityRecognizer'
  { -- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
    entityRecognizerArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEntityRecognizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityRecognizerArn', 'describeEntityRecognizer_entityRecognizerArn' - The Amazon Resource Name (ARN) that identifies the entity recognizer.
newDescribeEntityRecognizer ::
  -- | 'entityRecognizerArn'
  Core.Text ->
  DescribeEntityRecognizer
newDescribeEntityRecognizer pEntityRecognizerArn_ =
  DescribeEntityRecognizer'
    { entityRecognizerArn =
        pEntityRecognizerArn_
    }

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
describeEntityRecognizer_entityRecognizerArn :: Lens.Lens' DescribeEntityRecognizer Core.Text
describeEntityRecognizer_entityRecognizerArn = Lens.lens (\DescribeEntityRecognizer' {entityRecognizerArn} -> entityRecognizerArn) (\s@DescribeEntityRecognizer' {} a -> s {entityRecognizerArn = a} :: DescribeEntityRecognizer)

instance Core.AWSRequest DescribeEntityRecognizer where
  type
    AWSResponse DescribeEntityRecognizer =
      DescribeEntityRecognizerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEntityRecognizerResponse'
            Core.<$> (x Core..?> "EntityRecognizerProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEntityRecognizer

instance Core.NFData DescribeEntityRecognizer

instance Core.ToHeaders DescribeEntityRecognizer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DescribeEntityRecognizer" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEntityRecognizer where
  toJSON DescribeEntityRecognizer' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("EntityRecognizerArn" Core..= entityRecognizerArn)
          ]
      )

instance Core.ToPath DescribeEntityRecognizer where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEntityRecognizer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeEntityRecognizerResponse' smart constructor.
data DescribeEntityRecognizerResponse = DescribeEntityRecognizerResponse'
  { -- | Describes information associated with an entity recognizer.
    entityRecognizerProperties :: Core.Maybe EntityRecognizerProperties,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEntityRecognizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityRecognizerProperties', 'describeEntityRecognizerResponse_entityRecognizerProperties' - Describes information associated with an entity recognizer.
--
-- 'httpStatus', 'describeEntityRecognizerResponse_httpStatus' - The response's http status code.
newDescribeEntityRecognizerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEntityRecognizerResponse
newDescribeEntityRecognizerResponse pHttpStatus_ =
  DescribeEntityRecognizerResponse'
    { entityRecognizerProperties =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes information associated with an entity recognizer.
describeEntityRecognizerResponse_entityRecognizerProperties :: Lens.Lens' DescribeEntityRecognizerResponse (Core.Maybe EntityRecognizerProperties)
describeEntityRecognizerResponse_entityRecognizerProperties = Lens.lens (\DescribeEntityRecognizerResponse' {entityRecognizerProperties} -> entityRecognizerProperties) (\s@DescribeEntityRecognizerResponse' {} a -> s {entityRecognizerProperties = a} :: DescribeEntityRecognizerResponse)

-- | The response's http status code.
describeEntityRecognizerResponse_httpStatus :: Lens.Lens' DescribeEntityRecognizerResponse Core.Int
describeEntityRecognizerResponse_httpStatus = Lens.lens (\DescribeEntityRecognizerResponse' {httpStatus} -> httpStatus) (\s@DescribeEntityRecognizerResponse' {} a -> s {httpStatus = a} :: DescribeEntityRecognizerResponse)

instance Core.NFData DescribeEntityRecognizerResponse
