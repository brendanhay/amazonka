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
-- Module      : Amazonka.Comprehend.DescribeFlywheelIteration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the configuration properties of a flywheel iteration. For more
-- information about flywheels, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/flywheels-about.html Flywheel overview>
-- in the /Amazon Comprehend Developer Guide/.
module Amazonka.Comprehend.DescribeFlywheelIteration
  ( -- * Creating a Request
    DescribeFlywheelIteration (..),
    newDescribeFlywheelIteration,

    -- * Request Lenses
    describeFlywheelIteration_flywheelArn,
    describeFlywheelIteration_flywheelIterationId,

    -- * Destructuring the Response
    DescribeFlywheelIterationResponse (..),
    newDescribeFlywheelIterationResponse,

    -- * Response Lenses
    describeFlywheelIterationResponse_flywheelIterationProperties,
    describeFlywheelIterationResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFlywheelIteration' smart constructor.
data DescribeFlywheelIteration = DescribeFlywheelIteration'
  { flywheelArn :: Prelude.Text,
    flywheelIterationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFlywheelIteration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flywheelArn', 'describeFlywheelIteration_flywheelArn' -
--
-- 'flywheelIterationId', 'describeFlywheelIteration_flywheelIterationId' -
newDescribeFlywheelIteration ::
  -- | 'flywheelArn'
  Prelude.Text ->
  -- | 'flywheelIterationId'
  Prelude.Text ->
  DescribeFlywheelIteration
newDescribeFlywheelIteration
  pFlywheelArn_
  pFlywheelIterationId_ =
    DescribeFlywheelIteration'
      { flywheelArn =
          pFlywheelArn_,
        flywheelIterationId = pFlywheelIterationId_
      }

describeFlywheelIteration_flywheelArn :: Lens.Lens' DescribeFlywheelIteration Prelude.Text
describeFlywheelIteration_flywheelArn = Lens.lens (\DescribeFlywheelIteration' {flywheelArn} -> flywheelArn) (\s@DescribeFlywheelIteration' {} a -> s {flywheelArn = a} :: DescribeFlywheelIteration)

describeFlywheelIteration_flywheelIterationId :: Lens.Lens' DescribeFlywheelIteration Prelude.Text
describeFlywheelIteration_flywheelIterationId = Lens.lens (\DescribeFlywheelIteration' {flywheelIterationId} -> flywheelIterationId) (\s@DescribeFlywheelIteration' {} a -> s {flywheelIterationId = a} :: DescribeFlywheelIteration)

instance Core.AWSRequest DescribeFlywheelIteration where
  type
    AWSResponse DescribeFlywheelIteration =
      DescribeFlywheelIterationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFlywheelIterationResponse'
            Prelude.<$> (x Data..?> "FlywheelIterationProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFlywheelIteration where
  hashWithSalt _salt DescribeFlywheelIteration' {..} =
    _salt
      `Prelude.hashWithSalt` flywheelArn
      `Prelude.hashWithSalt` flywheelIterationId

instance Prelude.NFData DescribeFlywheelIteration where
  rnf DescribeFlywheelIteration' {..} =
    Prelude.rnf flywheelArn
      `Prelude.seq` Prelude.rnf flywheelIterationId

instance Data.ToHeaders DescribeFlywheelIteration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.DescribeFlywheelIteration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFlywheelIteration where
  toJSON DescribeFlywheelIteration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FlywheelArn" Data..= flywheelArn),
            Prelude.Just
              ("FlywheelIterationId" Data..= flywheelIterationId)
          ]
      )

instance Data.ToPath DescribeFlywheelIteration where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFlywheelIteration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFlywheelIterationResponse' smart constructor.
data DescribeFlywheelIterationResponse = DescribeFlywheelIterationResponse'
  { -- | The configuration properties of a flywheel iteration.
    flywheelIterationProperties :: Prelude.Maybe FlywheelIterationProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFlywheelIterationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flywheelIterationProperties', 'describeFlywheelIterationResponse_flywheelIterationProperties' - The configuration properties of a flywheel iteration.
--
-- 'httpStatus', 'describeFlywheelIterationResponse_httpStatus' - The response's http status code.
newDescribeFlywheelIterationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFlywheelIterationResponse
newDescribeFlywheelIterationResponse pHttpStatus_ =
  DescribeFlywheelIterationResponse'
    { flywheelIterationProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The configuration properties of a flywheel iteration.
describeFlywheelIterationResponse_flywheelIterationProperties :: Lens.Lens' DescribeFlywheelIterationResponse (Prelude.Maybe FlywheelIterationProperties)
describeFlywheelIterationResponse_flywheelIterationProperties = Lens.lens (\DescribeFlywheelIterationResponse' {flywheelIterationProperties} -> flywheelIterationProperties) (\s@DescribeFlywheelIterationResponse' {} a -> s {flywheelIterationProperties = a} :: DescribeFlywheelIterationResponse)

-- | The response's http status code.
describeFlywheelIterationResponse_httpStatus :: Lens.Lens' DescribeFlywheelIterationResponse Prelude.Int
describeFlywheelIterationResponse_httpStatus = Lens.lens (\DescribeFlywheelIterationResponse' {httpStatus} -> httpStatus) (\s@DescribeFlywheelIterationResponse' {} a -> s {httpStatus = a} :: DescribeFlywheelIterationResponse)

instance
  Prelude.NFData
    DescribeFlywheelIterationResponse
  where
  rnf DescribeFlywheelIterationResponse' {..} =
    Prelude.rnf flywheelIterationProperties
      `Prelude.seq` Prelude.rnf httpStatus
