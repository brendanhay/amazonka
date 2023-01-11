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
-- Module      : Amazonka.Personalize.DescribeFeatureTransformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the given feature transformation.
module Amazonka.Personalize.DescribeFeatureTransformation
  ( -- * Creating a Request
    DescribeFeatureTransformation (..),
    newDescribeFeatureTransformation,

    -- * Request Lenses
    describeFeatureTransformation_featureTransformationArn,

    -- * Destructuring the Response
    DescribeFeatureTransformationResponse (..),
    newDescribeFeatureTransformationResponse,

    -- * Response Lenses
    describeFeatureTransformationResponse_featureTransformation,
    describeFeatureTransformationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFeatureTransformation' smart constructor.
data DescribeFeatureTransformation = DescribeFeatureTransformation'
  { -- | The Amazon Resource Name (ARN) of the feature transformation to
    -- describe.
    featureTransformationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFeatureTransformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureTransformationArn', 'describeFeatureTransformation_featureTransformationArn' - The Amazon Resource Name (ARN) of the feature transformation to
-- describe.
newDescribeFeatureTransformation ::
  -- | 'featureTransformationArn'
  Prelude.Text ->
  DescribeFeatureTransformation
newDescribeFeatureTransformation
  pFeatureTransformationArn_ =
    DescribeFeatureTransformation'
      { featureTransformationArn =
          pFeatureTransformationArn_
      }

-- | The Amazon Resource Name (ARN) of the feature transformation to
-- describe.
describeFeatureTransformation_featureTransformationArn :: Lens.Lens' DescribeFeatureTransformation Prelude.Text
describeFeatureTransformation_featureTransformationArn = Lens.lens (\DescribeFeatureTransformation' {featureTransformationArn} -> featureTransformationArn) (\s@DescribeFeatureTransformation' {} a -> s {featureTransformationArn = a} :: DescribeFeatureTransformation)

instance
  Core.AWSRequest
    DescribeFeatureTransformation
  where
  type
    AWSResponse DescribeFeatureTransformation =
      DescribeFeatureTransformationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFeatureTransformationResponse'
            Prelude.<$> (x Data..?> "featureTransformation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeFeatureTransformation
  where
  hashWithSalt _salt DescribeFeatureTransformation' {..} =
    _salt
      `Prelude.hashWithSalt` featureTransformationArn

instance Prelude.NFData DescribeFeatureTransformation where
  rnf DescribeFeatureTransformation' {..} =
    Prelude.rnf featureTransformationArn

instance Data.ToHeaders DescribeFeatureTransformation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.DescribeFeatureTransformation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFeatureTransformation where
  toJSON DescribeFeatureTransformation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "featureTransformationArn"
                  Data..= featureTransformationArn
              )
          ]
      )

instance Data.ToPath DescribeFeatureTransformation where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFeatureTransformation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFeatureTransformationResponse' smart constructor.
data DescribeFeatureTransformationResponse = DescribeFeatureTransformationResponse'
  { -- | A listing of the FeatureTransformation properties.
    featureTransformation :: Prelude.Maybe FeatureTransformation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFeatureTransformationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureTransformation', 'describeFeatureTransformationResponse_featureTransformation' - A listing of the FeatureTransformation properties.
--
-- 'httpStatus', 'describeFeatureTransformationResponse_httpStatus' - The response's http status code.
newDescribeFeatureTransformationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFeatureTransformationResponse
newDescribeFeatureTransformationResponse pHttpStatus_ =
  DescribeFeatureTransformationResponse'
    { featureTransformation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A listing of the FeatureTransformation properties.
describeFeatureTransformationResponse_featureTransformation :: Lens.Lens' DescribeFeatureTransformationResponse (Prelude.Maybe FeatureTransformation)
describeFeatureTransformationResponse_featureTransformation = Lens.lens (\DescribeFeatureTransformationResponse' {featureTransformation} -> featureTransformation) (\s@DescribeFeatureTransformationResponse' {} a -> s {featureTransformation = a} :: DescribeFeatureTransformationResponse)

-- | The response's http status code.
describeFeatureTransformationResponse_httpStatus :: Lens.Lens' DescribeFeatureTransformationResponse Prelude.Int
describeFeatureTransformationResponse_httpStatus = Lens.lens (\DescribeFeatureTransformationResponse' {httpStatus} -> httpStatus) (\s@DescribeFeatureTransformationResponse' {} a -> s {httpStatus = a} :: DescribeFeatureTransformationResponse)

instance
  Prelude.NFData
    DescribeFeatureTransformationResponse
  where
  rnf DescribeFeatureTransformationResponse' {..} =
    Prelude.rnf featureTransformation
      `Prelude.seq` Prelude.rnf httpStatus
