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
-- Module      : Amazonka.EC2.DescribeExportTasks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified export instance tasks or all of your export
-- instance tasks.
module Amazonka.EC2.DescribeExportTasks
  ( -- * Creating a Request
    DescribeExportTasks (..),
    newDescribeExportTasks,

    -- * Request Lenses
    describeExportTasks_exportTaskIds,
    describeExportTasks_filters,

    -- * Destructuring the Response
    DescribeExportTasksResponse (..),
    newDescribeExportTasksResponse,

    -- * Response Lenses
    describeExportTasksResponse_exportTasks,
    describeExportTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { -- | The export task IDs.
    exportTaskIds :: Prelude.Maybe [Prelude.Text],
    -- | the filters for the export tasks.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExportTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportTaskIds', 'describeExportTasks_exportTaskIds' - The export task IDs.
--
-- 'filters', 'describeExportTasks_filters' - the filters for the export tasks.
newDescribeExportTasks ::
  DescribeExportTasks
newDescribeExportTasks =
  DescribeExportTasks'
    { exportTaskIds =
        Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The export task IDs.
describeExportTasks_exportTaskIds :: Lens.Lens' DescribeExportTasks (Prelude.Maybe [Prelude.Text])
describeExportTasks_exportTaskIds = Lens.lens (\DescribeExportTasks' {exportTaskIds} -> exportTaskIds) (\s@DescribeExportTasks' {} a -> s {exportTaskIds = a} :: DescribeExportTasks) Prelude.. Lens.mapping Lens.coerced

-- | the filters for the export tasks.
describeExportTasks_filters :: Lens.Lens' DescribeExportTasks (Prelude.Maybe [Filter])
describeExportTasks_filters = Lens.lens (\DescribeExportTasks' {filters} -> filters) (\s@DescribeExportTasks' {} a -> s {filters = a} :: DescribeExportTasks) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest DescribeExportTasks where
  type
    AWSResponse DescribeExportTasks =
      DescribeExportTasksResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeExportTasksResponse'
            Prelude.<$> ( x Data..@? "exportTaskSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeExportTasks where
  hashWithSalt _salt DescribeExportTasks' {..} =
    _salt
      `Prelude.hashWithSalt` exportTaskIds
      `Prelude.hashWithSalt` filters

instance Prelude.NFData DescribeExportTasks where
  rnf DescribeExportTasks' {..} =
    Prelude.rnf exportTaskIds `Prelude.seq`
      Prelude.rnf filters

instance Data.ToHeaders DescribeExportTasks where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeExportTasks where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeExportTasks where
  toQuery DescribeExportTasks' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeExportTasks" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "ExportTaskId"
              Prelude.<$> exportTaskIds
          ),
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { -- | Information about the export tasks.
    exportTasks :: Prelude.Maybe [ExportTask],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExportTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportTasks', 'describeExportTasksResponse_exportTasks' - Information about the export tasks.
--
-- 'httpStatus', 'describeExportTasksResponse_httpStatus' - The response's http status code.
newDescribeExportTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeExportTasksResponse
newDescribeExportTasksResponse pHttpStatus_ =
  DescribeExportTasksResponse'
    { exportTasks =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the export tasks.
describeExportTasksResponse_exportTasks :: Lens.Lens' DescribeExportTasksResponse (Prelude.Maybe [ExportTask])
describeExportTasksResponse_exportTasks = Lens.lens (\DescribeExportTasksResponse' {exportTasks} -> exportTasks) (\s@DescribeExportTasksResponse' {} a -> s {exportTasks = a} :: DescribeExportTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeExportTasksResponse_httpStatus :: Lens.Lens' DescribeExportTasksResponse Prelude.Int
describeExportTasksResponse_httpStatus = Lens.lens (\DescribeExportTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeExportTasksResponse' {} a -> s {httpStatus = a} :: DescribeExportTasksResponse)

instance Prelude.NFData DescribeExportTasksResponse where
  rnf DescribeExportTasksResponse' {..} =
    Prelude.rnf exportTasks `Prelude.seq`
      Prelude.rnf httpStatus
