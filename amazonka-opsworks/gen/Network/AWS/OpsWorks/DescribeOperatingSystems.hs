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
-- Module      : Network.AWS.OpsWorks.DescribeOperatingSystems
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the operating systems that are supported by AWS OpsWorks
-- Stacks.
module Network.AWS.OpsWorks.DescribeOperatingSystems
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOperatingSystemsResponse'
            Prelude.<$> ( x Core..?> "OperatingSystems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOperatingSystems

instance Prelude.NFData DescribeOperatingSystems

instance Core.ToHeaders DescribeOperatingSystems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeOperatingSystems" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeOperatingSystems where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath DescribeOperatingSystems where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeOperatingSystems where
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
describeOperatingSystemsResponse_operatingSystems = Lens.lens (\DescribeOperatingSystemsResponse' {operatingSystems} -> operatingSystems) (\s@DescribeOperatingSystemsResponse' {} a -> s {operatingSystems = a} :: DescribeOperatingSystemsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeOperatingSystemsResponse_httpStatus :: Lens.Lens' DescribeOperatingSystemsResponse Prelude.Int
describeOperatingSystemsResponse_httpStatus = Lens.lens (\DescribeOperatingSystemsResponse' {httpStatus} -> httpStatus) (\s@DescribeOperatingSystemsResponse' {} a -> s {httpStatus = a} :: DescribeOperatingSystemsResponse)

instance
  Prelude.NFData
    DescribeOperatingSystemsResponse
