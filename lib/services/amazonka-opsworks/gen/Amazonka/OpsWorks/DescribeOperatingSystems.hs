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
-- Module      : Amazonka.OpsWorks.DescribeOperatingSystems
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the operating systems that are supported by AWS OpsWorks
-- Stacks.
module Amazonka.OpsWorks.DescribeOperatingSystems
  ( -- * Creating a Request
    DescribeOperatingSystems (..),
    newDescribeOperatingSystems,

    -- * Destructuring the Response
    DescribeOperatingSystemsResponse (..),
    newDescribeOperatingSystemsResponse,

    -- * Response Lenses
    describeOperatingSystemsResponse_operatingSystems,
    describeOperatingSystemsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeOperatingSystems' smart constructor.
data DescribeOperatingSystems = DescribeOperatingSystems'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOperatingSystems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeOperatingSystems ::
  DescribeOperatingSystems
newDescribeOperatingSystems =
  DescribeOperatingSystems'

instance Core.AWSRequest DescribeOperatingSystems where
  type
    AWSResponse DescribeOperatingSystems =
      DescribeOperatingSystemsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOperatingSystemsResponse'
            Prelude.<$> ( x
                            Data..?> "OperatingSystems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOperatingSystems where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeOperatingSystems where
  rnf _ = ()

instance Data.ToHeaders DescribeOperatingSystems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DescribeOperatingSystems" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeOperatingSystems where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DescribeOperatingSystems where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeOperatingSystems where
  toQuery = Prelude.const Prelude.mempty

-- | The response to a @DescribeOperatingSystems@ request.
--
-- /See:/ 'newDescribeOperatingSystemsResponse' smart constructor.
data DescribeOperatingSystemsResponse = DescribeOperatingSystemsResponse'
  { -- | Contains information in response to a @DescribeOperatingSystems@
    -- request.
    operatingSystems :: Prelude.Maybe [OperatingSystem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOperatingSystemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operatingSystems', 'describeOperatingSystemsResponse_operatingSystems' - Contains information in response to a @DescribeOperatingSystems@
-- request.
--
-- 'httpStatus', 'describeOperatingSystemsResponse_httpStatus' - The response's http status code.
newDescribeOperatingSystemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOperatingSystemsResponse
newDescribeOperatingSystemsResponse pHttpStatus_ =
  DescribeOperatingSystemsResponse'
    { operatingSystems =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains information in response to a @DescribeOperatingSystems@
-- request.
describeOperatingSystemsResponse_operatingSystems :: Lens.Lens' DescribeOperatingSystemsResponse (Prelude.Maybe [OperatingSystem])
describeOperatingSystemsResponse_operatingSystems = Lens.lens (\DescribeOperatingSystemsResponse' {operatingSystems} -> operatingSystems) (\s@DescribeOperatingSystemsResponse' {} a -> s {operatingSystems = a} :: DescribeOperatingSystemsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeOperatingSystemsResponse_httpStatus :: Lens.Lens' DescribeOperatingSystemsResponse Prelude.Int
describeOperatingSystemsResponse_httpStatus = Lens.lens (\DescribeOperatingSystemsResponse' {httpStatus} -> httpStatus) (\s@DescribeOperatingSystemsResponse' {} a -> s {httpStatus = a} :: DescribeOperatingSystemsResponse)

instance
  Prelude.NFData
    DescribeOperatingSystemsResponse
  where
  rnf DescribeOperatingSystemsResponse' {..} =
    Prelude.rnf operatingSystems `Prelude.seq`
      Prelude.rnf httpStatus
