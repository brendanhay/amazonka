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
-- Module      : Network.AWS.EC2.DescribeConversionTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified conversion tasks or all your conversion tasks.
-- For more information, see the
-- <https://docs.aws.amazon.com/vm-import/latest/userguide/ VM Import\/Export User Guide>.
--
-- For information about the import manifest referenced by this API action,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest>.
module Network.AWS.EC2.DescribeConversionTasks
  ( -- * Creating a Request
    DescribeConversionTasks (..),
    newDescribeConversionTasks,

    -- * Request Lenses
    describeConversionTasks_conversionTaskIds,
    describeConversionTasks_dryRun,

    -- * Destructuring the Response
    DescribeConversionTasksResponse (..),
    newDescribeConversionTasksResponse,

    -- * Response Lenses
    describeConversionTasksResponse_conversionTasks,
    describeConversionTasksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeConversionTasks' smart constructor.
data DescribeConversionTasks = DescribeConversionTasks'
  { -- | The conversion task IDs.
    conversionTaskIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConversionTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conversionTaskIds', 'describeConversionTasks_conversionTaskIds' - The conversion task IDs.
--
-- 'dryRun', 'describeConversionTasks_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newDescribeConversionTasks ::
  DescribeConversionTasks
newDescribeConversionTasks =
  DescribeConversionTasks'
    { conversionTaskIds =
        Prelude.Nothing,
      dryRun = Prelude.Nothing
    }

-- | The conversion task IDs.
describeConversionTasks_conversionTaskIds :: Lens.Lens' DescribeConversionTasks (Prelude.Maybe [Prelude.Text])
describeConversionTasks_conversionTaskIds = Lens.lens (\DescribeConversionTasks' {conversionTaskIds} -> conversionTaskIds) (\s@DescribeConversionTasks' {} a -> s {conversionTaskIds = a} :: DescribeConversionTasks) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeConversionTasks_dryRun :: Lens.Lens' DescribeConversionTasks (Prelude.Maybe Prelude.Bool)
describeConversionTasks_dryRun = Lens.lens (\DescribeConversionTasks' {dryRun} -> dryRun) (\s@DescribeConversionTasks' {} a -> s {dryRun = a} :: DescribeConversionTasks)

instance Core.AWSRequest DescribeConversionTasks where
  type
    AWSResponse DescribeConversionTasks =
      DescribeConversionTasksResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeConversionTasksResponse'
            Prelude.<$> ( x Core..@? "conversionTasks" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConversionTasks

instance Prelude.NFData DescribeConversionTasks

instance Core.ToHeaders DescribeConversionTasks where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeConversionTasks where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeConversionTasks where
  toQuery DescribeConversionTasks' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeConversionTasks" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "ConversionTaskId"
              Prelude.<$> conversionTaskIds
          ),
        "DryRun" Core.=: dryRun
      ]

-- | /See:/ 'newDescribeConversionTasksResponse' smart constructor.
data DescribeConversionTasksResponse = DescribeConversionTasksResponse'
  { -- | Information about the conversion tasks.
    conversionTasks :: Prelude.Maybe [ConversionTask],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConversionTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conversionTasks', 'describeConversionTasksResponse_conversionTasks' - Information about the conversion tasks.
--
-- 'httpStatus', 'describeConversionTasksResponse_httpStatus' - The response's http status code.
newDescribeConversionTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConversionTasksResponse
newDescribeConversionTasksResponse pHttpStatus_ =
  DescribeConversionTasksResponse'
    { conversionTasks =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the conversion tasks.
describeConversionTasksResponse_conversionTasks :: Lens.Lens' DescribeConversionTasksResponse (Prelude.Maybe [ConversionTask])
describeConversionTasksResponse_conversionTasks = Lens.lens (\DescribeConversionTasksResponse' {conversionTasks} -> conversionTasks) (\s@DescribeConversionTasksResponse' {} a -> s {conversionTasks = a} :: DescribeConversionTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeConversionTasksResponse_httpStatus :: Lens.Lens' DescribeConversionTasksResponse Prelude.Int
describeConversionTasksResponse_httpStatus = Lens.lens (\DescribeConversionTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeConversionTasksResponse' {} a -> s {httpStatus = a} :: DescribeConversionTasksResponse)

instance
  Prelude.NFData
    DescribeConversionTasksResponse
