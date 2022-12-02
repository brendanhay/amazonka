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
-- Module      : Amazonka.EC2.DescribeConversionTasks
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.EC2.DescribeConversionTasks
  ( -- * Creating a Request
    DescribeConversionTasks (..),
    newDescribeConversionTasks,

    -- * Request Lenses
    describeConversionTasks_dryRun,
    describeConversionTasks_conversionTaskIds,

    -- * Destructuring the Response
    DescribeConversionTasksResponse (..),
    newDescribeConversionTasksResponse,

    -- * Response Lenses
    describeConversionTasksResponse_conversionTasks,
    describeConversionTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeConversionTasks' smart constructor.
data DescribeConversionTasks = DescribeConversionTasks'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The conversion task IDs.
    conversionTaskIds :: Prelude.Maybe [Prelude.Text]
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
-- 'dryRun', 'describeConversionTasks_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'conversionTaskIds', 'describeConversionTasks_conversionTaskIds' - The conversion task IDs.
newDescribeConversionTasks ::
  DescribeConversionTasks
newDescribeConversionTasks =
  DescribeConversionTasks'
    { dryRun = Prelude.Nothing,
      conversionTaskIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeConversionTasks_dryRun :: Lens.Lens' DescribeConversionTasks (Prelude.Maybe Prelude.Bool)
describeConversionTasks_dryRun = Lens.lens (\DescribeConversionTasks' {dryRun} -> dryRun) (\s@DescribeConversionTasks' {} a -> s {dryRun = a} :: DescribeConversionTasks)

-- | The conversion task IDs.
describeConversionTasks_conversionTaskIds :: Lens.Lens' DescribeConversionTasks (Prelude.Maybe [Prelude.Text])
describeConversionTasks_conversionTaskIds = Lens.lens (\DescribeConversionTasks' {conversionTaskIds} -> conversionTaskIds) (\s@DescribeConversionTasks' {} a -> s {conversionTaskIds = a} :: DescribeConversionTasks) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest DescribeConversionTasks where
  type
    AWSResponse DescribeConversionTasks =
      DescribeConversionTasksResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeConversionTasksResponse'
            Prelude.<$> ( x Data..@? "conversionTasks" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConversionTasks where
  hashWithSalt _salt DescribeConversionTasks' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` conversionTaskIds

instance Prelude.NFData DescribeConversionTasks where
  rnf DescribeConversionTasks' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf conversionTaskIds

instance Data.ToHeaders DescribeConversionTasks where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeConversionTasks where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeConversionTasks where
  toQuery DescribeConversionTasks' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeConversionTasks" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "ConversionTaskId"
              Prelude.<$> conversionTaskIds
          )
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
  where
  rnf DescribeConversionTasksResponse' {..} =
    Prelude.rnf conversionTasks
      `Prelude.seq` Prelude.rnf httpStatus
