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
-- Module      : Amazonka.Comprehend.DescribeEntityRecognizer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about an entity recognizer including status, S3 buckets
-- containing training data, recognizer metadata, metrics, and so on.
module Amazonka.Comprehend.DescribeEntityRecognizer
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

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEntityRecognizer' smart constructor.
data DescribeEntityRecognizer = DescribeEntityRecognizer'
  { -- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
    entityRecognizerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeEntityRecognizer
newDescribeEntityRecognizer pEntityRecognizerArn_ =
  DescribeEntityRecognizer'
    { entityRecognizerArn =
        pEntityRecognizerArn_
    }

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
describeEntityRecognizer_entityRecognizerArn :: Lens.Lens' DescribeEntityRecognizer Prelude.Text
describeEntityRecognizer_entityRecognizerArn = Lens.lens (\DescribeEntityRecognizer' {entityRecognizerArn} -> entityRecognizerArn) (\s@DescribeEntityRecognizer' {} a -> s {entityRecognizerArn = a} :: DescribeEntityRecognizer)

instance Core.AWSRequest DescribeEntityRecognizer where
  type
    AWSResponse DescribeEntityRecognizer =
      DescribeEntityRecognizerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEntityRecognizerResponse'
            Prelude.<$> (x Data..?> "EntityRecognizerProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEntityRecognizer where
  hashWithSalt _salt DescribeEntityRecognizer' {..} =
    _salt `Prelude.hashWithSalt` entityRecognizerArn

instance Prelude.NFData DescribeEntityRecognizer where
  rnf DescribeEntityRecognizer' {..} =
    Prelude.rnf entityRecognizerArn

instance Data.ToHeaders DescribeEntityRecognizer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.DescribeEntityRecognizer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEntityRecognizer where
  toJSON DescribeEntityRecognizer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EntityRecognizerArn" Data..= entityRecognizerArn)
          ]
      )

instance Data.ToPath DescribeEntityRecognizer where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEntityRecognizer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEntityRecognizerResponse' smart constructor.
data DescribeEntityRecognizerResponse = DescribeEntityRecognizerResponse'
  { -- | Describes information associated with an entity recognizer.
    entityRecognizerProperties :: Prelude.Maybe EntityRecognizerProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeEntityRecognizerResponse
newDescribeEntityRecognizerResponse pHttpStatus_ =
  DescribeEntityRecognizerResponse'
    { entityRecognizerProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes information associated with an entity recognizer.
describeEntityRecognizerResponse_entityRecognizerProperties :: Lens.Lens' DescribeEntityRecognizerResponse (Prelude.Maybe EntityRecognizerProperties)
describeEntityRecognizerResponse_entityRecognizerProperties = Lens.lens (\DescribeEntityRecognizerResponse' {entityRecognizerProperties} -> entityRecognizerProperties) (\s@DescribeEntityRecognizerResponse' {} a -> s {entityRecognizerProperties = a} :: DescribeEntityRecognizerResponse)

-- | The response's http status code.
describeEntityRecognizerResponse_httpStatus :: Lens.Lens' DescribeEntityRecognizerResponse Prelude.Int
describeEntityRecognizerResponse_httpStatus = Lens.lens (\DescribeEntityRecognizerResponse' {httpStatus} -> httpStatus) (\s@DescribeEntityRecognizerResponse' {} a -> s {httpStatus = a} :: DescribeEntityRecognizerResponse)

instance
  Prelude.NFData
    DescribeEntityRecognizerResponse
  where
  rnf DescribeEntityRecognizerResponse' {..} =
    Prelude.rnf entityRecognizerProperties `Prelude.seq`
      Prelude.rnf httpStatus
