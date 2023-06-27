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
-- Module      : Amazonka.Comprehend.DescribeFlywheel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides configuration information about the flywheel. For more
-- information about flywheels, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/flywheels-about.html Flywheel overview>
-- in the /Amazon Comprehend Developer Guide/.
module Amazonka.Comprehend.DescribeFlywheel
  ( -- * Creating a Request
    DescribeFlywheel (..),
    newDescribeFlywheel,

    -- * Request Lenses
    describeFlywheel_flywheelArn,

    -- * Destructuring the Response
    DescribeFlywheelResponse (..),
    newDescribeFlywheelResponse,

    -- * Response Lenses
    describeFlywheelResponse_flywheelProperties,
    describeFlywheelResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFlywheel' smart constructor.
data DescribeFlywheel = DescribeFlywheel'
  { -- | The Amazon Resource Number (ARN) of the flywheel.
    flywheelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFlywheel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flywheelArn', 'describeFlywheel_flywheelArn' - The Amazon Resource Number (ARN) of the flywheel.
newDescribeFlywheel ::
  -- | 'flywheelArn'
  Prelude.Text ->
  DescribeFlywheel
newDescribeFlywheel pFlywheelArn_ =
  DescribeFlywheel' {flywheelArn = pFlywheelArn_}

-- | The Amazon Resource Number (ARN) of the flywheel.
describeFlywheel_flywheelArn :: Lens.Lens' DescribeFlywheel Prelude.Text
describeFlywheel_flywheelArn = Lens.lens (\DescribeFlywheel' {flywheelArn} -> flywheelArn) (\s@DescribeFlywheel' {} a -> s {flywheelArn = a} :: DescribeFlywheel)

instance Core.AWSRequest DescribeFlywheel where
  type
    AWSResponse DescribeFlywheel =
      DescribeFlywheelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFlywheelResponse'
            Prelude.<$> (x Data..?> "FlywheelProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFlywheel where
  hashWithSalt _salt DescribeFlywheel' {..} =
    _salt `Prelude.hashWithSalt` flywheelArn

instance Prelude.NFData DescribeFlywheel where
  rnf DescribeFlywheel' {..} = Prelude.rnf flywheelArn

instance Data.ToHeaders DescribeFlywheel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.DescribeFlywheel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFlywheel where
  toJSON DescribeFlywheel' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("FlywheelArn" Data..= flywheelArn)]
      )

instance Data.ToPath DescribeFlywheel where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFlywheel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFlywheelResponse' smart constructor.
data DescribeFlywheelResponse = DescribeFlywheelResponse'
  { -- | The flywheel properties.
    flywheelProperties :: Prelude.Maybe FlywheelProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFlywheelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flywheelProperties', 'describeFlywheelResponse_flywheelProperties' - The flywheel properties.
--
-- 'httpStatus', 'describeFlywheelResponse_httpStatus' - The response's http status code.
newDescribeFlywheelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFlywheelResponse
newDescribeFlywheelResponse pHttpStatus_ =
  DescribeFlywheelResponse'
    { flywheelProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The flywheel properties.
describeFlywheelResponse_flywheelProperties :: Lens.Lens' DescribeFlywheelResponse (Prelude.Maybe FlywheelProperties)
describeFlywheelResponse_flywheelProperties = Lens.lens (\DescribeFlywheelResponse' {flywheelProperties} -> flywheelProperties) (\s@DescribeFlywheelResponse' {} a -> s {flywheelProperties = a} :: DescribeFlywheelResponse)

-- | The response's http status code.
describeFlywheelResponse_httpStatus :: Lens.Lens' DescribeFlywheelResponse Prelude.Int
describeFlywheelResponse_httpStatus = Lens.lens (\DescribeFlywheelResponse' {httpStatus} -> httpStatus) (\s@DescribeFlywheelResponse' {} a -> s {httpStatus = a} :: DescribeFlywheelResponse)

instance Prelude.NFData DescribeFlywheelResponse where
  rnf DescribeFlywheelResponse' {..} =
    Prelude.rnf flywheelProperties
      `Prelude.seq` Prelude.rnf httpStatus
