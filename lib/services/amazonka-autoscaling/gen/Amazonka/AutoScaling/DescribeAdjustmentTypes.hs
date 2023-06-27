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
-- Module      : Amazonka.AutoScaling.DescribeAdjustmentTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available adjustment types for step scaling and simple
-- scaling policies.
--
-- The following adjustment types are supported:
--
-- -   @ChangeInCapacity@
--
-- -   @ExactCapacity@
--
-- -   @PercentChangeInCapacity@
module Amazonka.AutoScaling.DescribeAdjustmentTypes
  ( -- * Creating a Request
    DescribeAdjustmentTypes (..),
    newDescribeAdjustmentTypes,

    -- * Destructuring the Response
    DescribeAdjustmentTypesResponse (..),
    newDescribeAdjustmentTypesResponse,

    -- * Response Lenses
    describeAdjustmentTypesResponse_adjustmentTypes,
    describeAdjustmentTypesResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAdjustmentTypes' smart constructor.
data DescribeAdjustmentTypes = DescribeAdjustmentTypes'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAdjustmentTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeAdjustmentTypes ::
  DescribeAdjustmentTypes
newDescribeAdjustmentTypes = DescribeAdjustmentTypes'

instance Core.AWSRequest DescribeAdjustmentTypes where
  type
    AWSResponse DescribeAdjustmentTypes =
      DescribeAdjustmentTypesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeAdjustmentTypesResult"
      ( \s h x ->
          DescribeAdjustmentTypesResponse'
            Prelude.<$> ( x
                            Data..@? "AdjustmentTypes"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAdjustmentTypes where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeAdjustmentTypes where
  rnf _ = ()

instance Data.ToHeaders DescribeAdjustmentTypes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeAdjustmentTypes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAdjustmentTypes where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ("DescribeAdjustmentTypes" :: Prelude.ByteString),
            "Version"
              Data.=: ("2011-01-01" :: Prelude.ByteString)
          ]
      )

-- | /See:/ 'newDescribeAdjustmentTypesResponse' smart constructor.
data DescribeAdjustmentTypesResponse = DescribeAdjustmentTypesResponse'
  { -- | The policy adjustment types.
    adjustmentTypes :: Prelude.Maybe [AdjustmentType],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAdjustmentTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adjustmentTypes', 'describeAdjustmentTypesResponse_adjustmentTypes' - The policy adjustment types.
--
-- 'httpStatus', 'describeAdjustmentTypesResponse_httpStatus' - The response's http status code.
newDescribeAdjustmentTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAdjustmentTypesResponse
newDescribeAdjustmentTypesResponse pHttpStatus_ =
  DescribeAdjustmentTypesResponse'
    { adjustmentTypes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The policy adjustment types.
describeAdjustmentTypesResponse_adjustmentTypes :: Lens.Lens' DescribeAdjustmentTypesResponse (Prelude.Maybe [AdjustmentType])
describeAdjustmentTypesResponse_adjustmentTypes = Lens.lens (\DescribeAdjustmentTypesResponse' {adjustmentTypes} -> adjustmentTypes) (\s@DescribeAdjustmentTypesResponse' {} a -> s {adjustmentTypes = a} :: DescribeAdjustmentTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAdjustmentTypesResponse_httpStatus :: Lens.Lens' DescribeAdjustmentTypesResponse Prelude.Int
describeAdjustmentTypesResponse_httpStatus = Lens.lens (\DescribeAdjustmentTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeAdjustmentTypesResponse' {} a -> s {httpStatus = a} :: DescribeAdjustmentTypesResponse)

instance
  Prelude.NFData
    DescribeAdjustmentTypesResponse
  where
  rnf DescribeAdjustmentTypesResponse' {..} =
    Prelude.rnf adjustmentTypes
      `Prelude.seq` Prelude.rnf httpStatus
