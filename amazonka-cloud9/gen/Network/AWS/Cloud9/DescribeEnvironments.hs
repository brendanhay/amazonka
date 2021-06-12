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
-- Module      : Network.AWS.Cloud9.DescribeEnvironments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about AWS Cloud9 development environments.
module Network.AWS.Cloud9.DescribeEnvironments
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

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEnvironments' smart constructor.
data DescribeEnvironments = DescribeEnvironments'
  { -- | The IDs of individual environments to get information about.
    environmentIds :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.NonEmpty Core.Text ->
  DescribeEnvironments
newDescribeEnvironments pEnvironmentIds_ =
  DescribeEnvironments'
    { environmentIds =
        Lens._Coerce Lens.# pEnvironmentIds_
    }

-- | The IDs of individual environments to get information about.
describeEnvironments_environmentIds :: Lens.Lens' DescribeEnvironments (Core.NonEmpty Core.Text)
describeEnvironments_environmentIds = Lens.lens (\DescribeEnvironments' {environmentIds} -> environmentIds) (\s@DescribeEnvironments' {} a -> s {environmentIds = a} :: DescribeEnvironments) Core.. Lens._Coerce

instance Core.AWSRequest DescribeEnvironments where
  type
    AWSResponse DescribeEnvironments =
      DescribeEnvironmentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEnvironmentsResponse'
            Core.<$> (x Core..?> "environments" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEnvironments

instance Core.NFData DescribeEnvironments

instance Core.ToHeaders DescribeEnvironments where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCloud9WorkspaceManagementService.DescribeEnvironments" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEnvironments where
  toJSON DescribeEnvironments' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("environmentIds" Core..= environmentIds)
          ]
      )

instance Core.ToPath DescribeEnvironments where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEnvironments where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeEnvironmentsResponse' smart constructor.
data DescribeEnvironmentsResponse = DescribeEnvironmentsResponse'
  { -- | Information about the environments that are returned.
    environments :: Core.Maybe [Environment],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeEnvironmentsResponse
newDescribeEnvironmentsResponse pHttpStatus_ =
  DescribeEnvironmentsResponse'
    { environments =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the environments that are returned.
describeEnvironmentsResponse_environments :: Lens.Lens' DescribeEnvironmentsResponse (Core.Maybe [Environment])
describeEnvironmentsResponse_environments = Lens.lens (\DescribeEnvironmentsResponse' {environments} -> environments) (\s@DescribeEnvironmentsResponse' {} a -> s {environments = a} :: DescribeEnvironmentsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEnvironmentsResponse_httpStatus :: Lens.Lens' DescribeEnvironmentsResponse Core.Int
describeEnvironmentsResponse_httpStatus = Lens.lens (\DescribeEnvironmentsResponse' {httpStatus} -> httpStatus) (\s@DescribeEnvironmentsResponse' {} a -> s {httpStatus = a} :: DescribeEnvironmentsResponse)

instance Core.NFData DescribeEnvironmentsResponse
