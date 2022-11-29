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
-- Module      : Amazonka.Cloud9.DescribeEnvironments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about Cloud9 development environments.
module Amazonka.Cloud9.DescribeEnvironments
  ( -- * Creating a Request
    DescribeEnvironments (..),
    newDescribeEnvironments,

    -- * Request Lenses
    describeEnvironments_environmentIds,

    -- * Destructuring the Response
    DescribeEnvironmentsResponse (..),
    newDescribeEnvironmentsResponse,

    -- * Response Lenses
    describeEnvironmentsResponse_environments,
    describeEnvironmentsResponse_httpStatus,
  )
where

import Amazonka.Cloud9.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEnvironments' smart constructor.
data DescribeEnvironments = DescribeEnvironments'
  { -- | The IDs of individual environments to get information about.
    environmentIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEnvironments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentIds', 'describeEnvironments_environmentIds' - The IDs of individual environments to get information about.
newDescribeEnvironments ::
  -- | 'environmentIds'
  Prelude.NonEmpty Prelude.Text ->
  DescribeEnvironments
newDescribeEnvironments pEnvironmentIds_ =
  DescribeEnvironments'
    { environmentIds =
        Lens.coerced Lens.# pEnvironmentIds_
    }

-- | The IDs of individual environments to get information about.
describeEnvironments_environmentIds :: Lens.Lens' DescribeEnvironments (Prelude.NonEmpty Prelude.Text)
describeEnvironments_environmentIds = Lens.lens (\DescribeEnvironments' {environmentIds} -> environmentIds) (\s@DescribeEnvironments' {} a -> s {environmentIds = a} :: DescribeEnvironments) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeEnvironments where
  type
    AWSResponse DescribeEnvironments =
      DescribeEnvironmentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEnvironmentsResponse'
            Prelude.<$> (x Core..?> "environments" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEnvironments where
  hashWithSalt _salt DescribeEnvironments' {..} =
    _salt `Prelude.hashWithSalt` environmentIds

instance Prelude.NFData DescribeEnvironments where
  rnf DescribeEnvironments' {..} =
    Prelude.rnf environmentIds

instance Core.ToHeaders DescribeEnvironments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCloud9WorkspaceManagementService.DescribeEnvironments" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeEnvironments where
  toJSON DescribeEnvironments' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("environmentIds" Core..= environmentIds)
          ]
      )

instance Core.ToPath DescribeEnvironments where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeEnvironments where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEnvironmentsResponse' smart constructor.
data DescribeEnvironmentsResponse = DescribeEnvironmentsResponse'
  { -- | Information about the environments that are returned.
    environments :: Prelude.Maybe [Environment],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEnvironmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environments', 'describeEnvironmentsResponse_environments' - Information about the environments that are returned.
--
-- 'httpStatus', 'describeEnvironmentsResponse_httpStatus' - The response's http status code.
newDescribeEnvironmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEnvironmentsResponse
newDescribeEnvironmentsResponse pHttpStatus_ =
  DescribeEnvironmentsResponse'
    { environments =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the environments that are returned.
describeEnvironmentsResponse_environments :: Lens.Lens' DescribeEnvironmentsResponse (Prelude.Maybe [Environment])
describeEnvironmentsResponse_environments = Lens.lens (\DescribeEnvironmentsResponse' {environments} -> environments) (\s@DescribeEnvironmentsResponse' {} a -> s {environments = a} :: DescribeEnvironmentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeEnvironmentsResponse_httpStatus :: Lens.Lens' DescribeEnvironmentsResponse Prelude.Int
describeEnvironmentsResponse_httpStatus = Lens.lens (\DescribeEnvironmentsResponse' {httpStatus} -> httpStatus) (\s@DescribeEnvironmentsResponse' {} a -> s {httpStatus = a} :: DescribeEnvironmentsResponse)

instance Prelude.NFData DescribeEnvironmentsResponse where
  rnf DescribeEnvironmentsResponse' {..} =
    Prelude.rnf environments
      `Prelude.seq` Prelude.rnf httpStatus
