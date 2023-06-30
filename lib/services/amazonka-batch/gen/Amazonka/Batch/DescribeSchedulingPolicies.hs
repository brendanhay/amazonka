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
-- Module      : Amazonka.Batch.DescribeSchedulingPolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your scheduling policies.
module Amazonka.Batch.DescribeSchedulingPolicies
  ( -- * Creating a Request
    DescribeSchedulingPolicies (..),
    newDescribeSchedulingPolicies,

    -- * Request Lenses
    describeSchedulingPolicies_arns,

    -- * Destructuring the Response
    DescribeSchedulingPoliciesResponse (..),
    newDescribeSchedulingPoliciesResponse,

    -- * Response Lenses
    describeSchedulingPoliciesResponse_schedulingPolicies,
    describeSchedulingPoliciesResponse_httpStatus,
  )
where

import Amazonka.Batch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for @DescribeSchedulingPolicies@.
--
-- /See:/ 'newDescribeSchedulingPolicies' smart constructor.
data DescribeSchedulingPolicies = DescribeSchedulingPolicies'
  { -- | A list of up to 100 scheduling policy Amazon Resource Name (ARN)
    -- entries.
    arns :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSchedulingPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arns', 'describeSchedulingPolicies_arns' - A list of up to 100 scheduling policy Amazon Resource Name (ARN)
-- entries.
newDescribeSchedulingPolicies ::
  DescribeSchedulingPolicies
newDescribeSchedulingPolicies =
  DescribeSchedulingPolicies' {arns = Prelude.mempty}

-- | A list of up to 100 scheduling policy Amazon Resource Name (ARN)
-- entries.
describeSchedulingPolicies_arns :: Lens.Lens' DescribeSchedulingPolicies [Prelude.Text]
describeSchedulingPolicies_arns = Lens.lens (\DescribeSchedulingPolicies' {arns} -> arns) (\s@DescribeSchedulingPolicies' {} a -> s {arns = a} :: DescribeSchedulingPolicies) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeSchedulingPolicies where
  type
    AWSResponse DescribeSchedulingPolicies =
      DescribeSchedulingPoliciesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSchedulingPoliciesResponse'
            Prelude.<$> ( x
                            Data..?> "schedulingPolicies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSchedulingPolicies where
  hashWithSalt _salt DescribeSchedulingPolicies' {..} =
    _salt `Prelude.hashWithSalt` arns

instance Prelude.NFData DescribeSchedulingPolicies where
  rnf DescribeSchedulingPolicies' {..} =
    Prelude.rnf arns

instance Data.ToHeaders DescribeSchedulingPolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSchedulingPolicies where
  toJSON DescribeSchedulingPolicies' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arns" Data..= arns)]
      )

instance Data.ToPath DescribeSchedulingPolicies where
  toPath =
    Prelude.const "/v1/describeschedulingpolicies"

instance Data.ToQuery DescribeSchedulingPolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSchedulingPoliciesResponse' smart constructor.
data DescribeSchedulingPoliciesResponse = DescribeSchedulingPoliciesResponse'
  { -- | The list of scheduling policies.
    schedulingPolicies :: Prelude.Maybe [SchedulingPolicyDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSchedulingPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schedulingPolicies', 'describeSchedulingPoliciesResponse_schedulingPolicies' - The list of scheduling policies.
--
-- 'httpStatus', 'describeSchedulingPoliciesResponse_httpStatus' - The response's http status code.
newDescribeSchedulingPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSchedulingPoliciesResponse
newDescribeSchedulingPoliciesResponse pHttpStatus_ =
  DescribeSchedulingPoliciesResponse'
    { schedulingPolicies =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of scheduling policies.
describeSchedulingPoliciesResponse_schedulingPolicies :: Lens.Lens' DescribeSchedulingPoliciesResponse (Prelude.Maybe [SchedulingPolicyDetail])
describeSchedulingPoliciesResponse_schedulingPolicies = Lens.lens (\DescribeSchedulingPoliciesResponse' {schedulingPolicies} -> schedulingPolicies) (\s@DescribeSchedulingPoliciesResponse' {} a -> s {schedulingPolicies = a} :: DescribeSchedulingPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSchedulingPoliciesResponse_httpStatus :: Lens.Lens' DescribeSchedulingPoliciesResponse Prelude.Int
describeSchedulingPoliciesResponse_httpStatus = Lens.lens (\DescribeSchedulingPoliciesResponse' {httpStatus} -> httpStatus) (\s@DescribeSchedulingPoliciesResponse' {} a -> s {httpStatus = a} :: DescribeSchedulingPoliciesResponse)

instance
  Prelude.NFData
    DescribeSchedulingPoliciesResponse
  where
  rnf DescribeSchedulingPoliciesResponse' {..} =
    Prelude.rnf schedulingPolicies
      `Prelude.seq` Prelude.rnf httpStatus
