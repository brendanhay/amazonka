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
-- Module      : Amazonka.ElasticInference.DescribeAcceleratorTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the accelerator types available in a given region, as well as
-- their characteristics, such as memory and throughput.
module Amazonka.ElasticInference.DescribeAcceleratorTypes
  ( -- * Creating a Request
    DescribeAcceleratorTypes (..),
    newDescribeAcceleratorTypes,

    -- * Destructuring the Response
    DescribeAcceleratorTypesResponse (..),
    newDescribeAcceleratorTypesResponse,

    -- * Response Lenses
    describeAcceleratorTypesResponse_acceleratorTypes,
    describeAcceleratorTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticInference.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAcceleratorTypes' smart constructor.
data DescribeAcceleratorTypes = DescribeAcceleratorTypes'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAcceleratorTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeAcceleratorTypes ::
  DescribeAcceleratorTypes
newDescribeAcceleratorTypes =
  DescribeAcceleratorTypes'

instance Core.AWSRequest DescribeAcceleratorTypes where
  type
    AWSResponse DescribeAcceleratorTypes =
      DescribeAcceleratorTypesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAcceleratorTypesResponse'
            Prelude.<$> ( x Data..?> "acceleratorTypes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAcceleratorTypes where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeAcceleratorTypes where
  rnf _ = ()

instance Data.ToHeaders DescribeAcceleratorTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeAcceleratorTypes where
  toPath = Prelude.const "/describe-accelerator-types"

instance Data.ToQuery DescribeAcceleratorTypes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAcceleratorTypesResponse' smart constructor.
data DescribeAcceleratorTypesResponse = DescribeAcceleratorTypesResponse'
  { -- | The available accelerator types.
    acceleratorTypes :: Prelude.Maybe [AcceleratorType],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAcceleratorTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorTypes', 'describeAcceleratorTypesResponse_acceleratorTypes' - The available accelerator types.
--
-- 'httpStatus', 'describeAcceleratorTypesResponse_httpStatus' - The response's http status code.
newDescribeAcceleratorTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAcceleratorTypesResponse
newDescribeAcceleratorTypesResponse pHttpStatus_ =
  DescribeAcceleratorTypesResponse'
    { acceleratorTypes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The available accelerator types.
describeAcceleratorTypesResponse_acceleratorTypes :: Lens.Lens' DescribeAcceleratorTypesResponse (Prelude.Maybe [AcceleratorType])
describeAcceleratorTypesResponse_acceleratorTypes = Lens.lens (\DescribeAcceleratorTypesResponse' {acceleratorTypes} -> acceleratorTypes) (\s@DescribeAcceleratorTypesResponse' {} a -> s {acceleratorTypes = a} :: DescribeAcceleratorTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAcceleratorTypesResponse_httpStatus :: Lens.Lens' DescribeAcceleratorTypesResponse Prelude.Int
describeAcceleratorTypesResponse_httpStatus = Lens.lens (\DescribeAcceleratorTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeAcceleratorTypesResponse' {} a -> s {httpStatus = a} :: DescribeAcceleratorTypesResponse)

instance
  Prelude.NFData
    DescribeAcceleratorTypesResponse
  where
  rnf DescribeAcceleratorTypesResponse' {..} =
    Prelude.rnf acceleratorTypes
      `Prelude.seq` Prelude.rnf httpStatus
