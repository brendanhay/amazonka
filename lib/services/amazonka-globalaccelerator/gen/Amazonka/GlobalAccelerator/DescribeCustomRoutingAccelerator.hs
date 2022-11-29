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
-- Module      : Amazonka.GlobalAccelerator.DescribeCustomRoutingAccelerator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe a custom routing accelerator.
module Amazonka.GlobalAccelerator.DescribeCustomRoutingAccelerator
  ( -- * Creating a Request
    DescribeCustomRoutingAccelerator (..),
    newDescribeCustomRoutingAccelerator,

    -- * Request Lenses
    describeCustomRoutingAccelerator_acceleratorArn,

    -- * Destructuring the Response
    DescribeCustomRoutingAcceleratorResponse (..),
    newDescribeCustomRoutingAcceleratorResponse,

    -- * Response Lenses
    describeCustomRoutingAcceleratorResponse_accelerator,
    describeCustomRoutingAcceleratorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCustomRoutingAccelerator' smart constructor.
data DescribeCustomRoutingAccelerator = DescribeCustomRoutingAccelerator'
  { -- | The Amazon Resource Name (ARN) of the accelerator to describe.
    acceleratorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomRoutingAccelerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorArn', 'describeCustomRoutingAccelerator_acceleratorArn' - The Amazon Resource Name (ARN) of the accelerator to describe.
newDescribeCustomRoutingAccelerator ::
  -- | 'acceleratorArn'
  Prelude.Text ->
  DescribeCustomRoutingAccelerator
newDescribeCustomRoutingAccelerator pAcceleratorArn_ =
  DescribeCustomRoutingAccelerator'
    { acceleratorArn =
        pAcceleratorArn_
    }

-- | The Amazon Resource Name (ARN) of the accelerator to describe.
describeCustomRoutingAccelerator_acceleratorArn :: Lens.Lens' DescribeCustomRoutingAccelerator Prelude.Text
describeCustomRoutingAccelerator_acceleratorArn = Lens.lens (\DescribeCustomRoutingAccelerator' {acceleratorArn} -> acceleratorArn) (\s@DescribeCustomRoutingAccelerator' {} a -> s {acceleratorArn = a} :: DescribeCustomRoutingAccelerator)

instance
  Core.AWSRequest
    DescribeCustomRoutingAccelerator
  where
  type
    AWSResponse DescribeCustomRoutingAccelerator =
      DescribeCustomRoutingAcceleratorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCustomRoutingAcceleratorResponse'
            Prelude.<$> (x Core..?> "Accelerator")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeCustomRoutingAccelerator
  where
  hashWithSalt
    _salt
    DescribeCustomRoutingAccelerator' {..} =
      _salt `Prelude.hashWithSalt` acceleratorArn

instance
  Prelude.NFData
    DescribeCustomRoutingAccelerator
  where
  rnf DescribeCustomRoutingAccelerator' {..} =
    Prelude.rnf acceleratorArn

instance
  Core.ToHeaders
    DescribeCustomRoutingAccelerator
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GlobalAccelerator_V20180706.DescribeCustomRoutingAccelerator" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeCustomRoutingAccelerator where
  toJSON DescribeCustomRoutingAccelerator' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AcceleratorArn" Core..= acceleratorArn)
          ]
      )

instance Core.ToPath DescribeCustomRoutingAccelerator where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeCustomRoutingAccelerator
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCustomRoutingAcceleratorResponse' smart constructor.
data DescribeCustomRoutingAcceleratorResponse = DescribeCustomRoutingAcceleratorResponse'
  { -- | The description of the custom routing accelerator.
    accelerator :: Prelude.Maybe CustomRoutingAccelerator,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomRoutingAcceleratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accelerator', 'describeCustomRoutingAcceleratorResponse_accelerator' - The description of the custom routing accelerator.
--
-- 'httpStatus', 'describeCustomRoutingAcceleratorResponse_httpStatus' - The response's http status code.
newDescribeCustomRoutingAcceleratorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCustomRoutingAcceleratorResponse
newDescribeCustomRoutingAcceleratorResponse
  pHttpStatus_ =
    DescribeCustomRoutingAcceleratorResponse'
      { accelerator =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The description of the custom routing accelerator.
describeCustomRoutingAcceleratorResponse_accelerator :: Lens.Lens' DescribeCustomRoutingAcceleratorResponse (Prelude.Maybe CustomRoutingAccelerator)
describeCustomRoutingAcceleratorResponse_accelerator = Lens.lens (\DescribeCustomRoutingAcceleratorResponse' {accelerator} -> accelerator) (\s@DescribeCustomRoutingAcceleratorResponse' {} a -> s {accelerator = a} :: DescribeCustomRoutingAcceleratorResponse)

-- | The response's http status code.
describeCustomRoutingAcceleratorResponse_httpStatus :: Lens.Lens' DescribeCustomRoutingAcceleratorResponse Prelude.Int
describeCustomRoutingAcceleratorResponse_httpStatus = Lens.lens (\DescribeCustomRoutingAcceleratorResponse' {httpStatus} -> httpStatus) (\s@DescribeCustomRoutingAcceleratorResponse' {} a -> s {httpStatus = a} :: DescribeCustomRoutingAcceleratorResponse)

instance
  Prelude.NFData
    DescribeCustomRoutingAcceleratorResponse
  where
  rnf DescribeCustomRoutingAcceleratorResponse' {..} =
    Prelude.rnf accelerator
      `Prelude.seq` Prelude.rnf httpStatus
