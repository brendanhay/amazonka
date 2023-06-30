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
-- Module      : Amazonka.AutoScaling.DescribeLifecycleHookTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available types of lifecycle hooks.
--
-- The following hook types are supported:
--
-- -   @autoscaling:EC2_INSTANCE_LAUNCHING@
--
-- -   @autoscaling:EC2_INSTANCE_TERMINATING@
module Amazonka.AutoScaling.DescribeLifecycleHookTypes
  ( -- * Creating a Request
    DescribeLifecycleHookTypes (..),
    newDescribeLifecycleHookTypes,

    -- * Destructuring the Response
    DescribeLifecycleHookTypesResponse (..),
    newDescribeLifecycleHookTypesResponse,

    -- * Response Lenses
    describeLifecycleHookTypesResponse_lifecycleHookTypes,
    describeLifecycleHookTypesResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLifecycleHookTypes' smart constructor.
data DescribeLifecycleHookTypes = DescribeLifecycleHookTypes'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLifecycleHookTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeLifecycleHookTypes ::
  DescribeLifecycleHookTypes
newDescribeLifecycleHookTypes =
  DescribeLifecycleHookTypes'

instance Core.AWSRequest DescribeLifecycleHookTypes where
  type
    AWSResponse DescribeLifecycleHookTypes =
      DescribeLifecycleHookTypesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeLifecycleHookTypesResult"
      ( \s h x ->
          DescribeLifecycleHookTypesResponse'
            Prelude.<$> ( x
                            Data..@? "LifecycleHookTypes"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLifecycleHookTypes where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeLifecycleHookTypes where
  rnf _ = ()

instance Data.ToHeaders DescribeLifecycleHookTypes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeLifecycleHookTypes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLifecycleHookTypes where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ("DescribeLifecycleHookTypes" :: Prelude.ByteString),
            "Version"
              Data.=: ("2011-01-01" :: Prelude.ByteString)
          ]
      )

-- | /See:/ 'newDescribeLifecycleHookTypesResponse' smart constructor.
data DescribeLifecycleHookTypesResponse = DescribeLifecycleHookTypesResponse'
  { -- | The lifecycle hook types.
    lifecycleHookTypes :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLifecycleHookTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycleHookTypes', 'describeLifecycleHookTypesResponse_lifecycleHookTypes' - The lifecycle hook types.
--
-- 'httpStatus', 'describeLifecycleHookTypesResponse_httpStatus' - The response's http status code.
newDescribeLifecycleHookTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLifecycleHookTypesResponse
newDescribeLifecycleHookTypesResponse pHttpStatus_ =
  DescribeLifecycleHookTypesResponse'
    { lifecycleHookTypes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The lifecycle hook types.
describeLifecycleHookTypesResponse_lifecycleHookTypes :: Lens.Lens' DescribeLifecycleHookTypesResponse (Prelude.Maybe [Prelude.Text])
describeLifecycleHookTypesResponse_lifecycleHookTypes = Lens.lens (\DescribeLifecycleHookTypesResponse' {lifecycleHookTypes} -> lifecycleHookTypes) (\s@DescribeLifecycleHookTypesResponse' {} a -> s {lifecycleHookTypes = a} :: DescribeLifecycleHookTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeLifecycleHookTypesResponse_httpStatus :: Lens.Lens' DescribeLifecycleHookTypesResponse Prelude.Int
describeLifecycleHookTypesResponse_httpStatus = Lens.lens (\DescribeLifecycleHookTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeLifecycleHookTypesResponse' {} a -> s {httpStatus = a} :: DescribeLifecycleHookTypesResponse)

instance
  Prelude.NFData
    DescribeLifecycleHookTypesResponse
  where
  rnf DescribeLifecycleHookTypesResponse' {..} =
    Prelude.rnf lifecycleHookTypes
      `Prelude.seq` Prelude.rnf httpStatus
