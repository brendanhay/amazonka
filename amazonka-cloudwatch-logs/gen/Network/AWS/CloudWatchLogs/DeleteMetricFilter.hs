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
-- Module      : Network.AWS.CloudWatchLogs.DeleteMetricFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified metric filter.
module Network.AWS.CloudWatchLogs.DeleteMetricFilter
  ( -- * Creating a Request
    DeleteMetricFilter (..),
    newDeleteMetricFilter,

    -- * Request Lenses
    deleteMetricFilter_logGroupName,
    deleteMetricFilter_filterName,

    -- * Destructuring the Response
    DeleteMetricFilterResponse (..),
    newDeleteMetricFilterResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteMetricFilter' smart constructor.
data DeleteMetricFilter = DeleteMetricFilter'
  { -- | The name of the log group.
    logGroupName :: Core.Text,
    -- | The name of the metric filter.
    filterName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteMetricFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'deleteMetricFilter_logGroupName' - The name of the log group.
--
-- 'filterName', 'deleteMetricFilter_filterName' - The name of the metric filter.
newDeleteMetricFilter ::
  -- | 'logGroupName'
  Core.Text ->
  -- | 'filterName'
  Core.Text ->
  DeleteMetricFilter
newDeleteMetricFilter pLogGroupName_ pFilterName_ =
  DeleteMetricFilter'
    { logGroupName = pLogGroupName_,
      filterName = pFilterName_
    }

-- | The name of the log group.
deleteMetricFilter_logGroupName :: Lens.Lens' DeleteMetricFilter Core.Text
deleteMetricFilter_logGroupName = Lens.lens (\DeleteMetricFilter' {logGroupName} -> logGroupName) (\s@DeleteMetricFilter' {} a -> s {logGroupName = a} :: DeleteMetricFilter)

-- | The name of the metric filter.
deleteMetricFilter_filterName :: Lens.Lens' DeleteMetricFilter Core.Text
deleteMetricFilter_filterName = Lens.lens (\DeleteMetricFilter' {filterName} -> filterName) (\s@DeleteMetricFilter' {} a -> s {filterName = a} :: DeleteMetricFilter)

instance Core.AWSRequest DeleteMetricFilter where
  type
    AWSResponse DeleteMetricFilter =
      DeleteMetricFilterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteMetricFilterResponse'

instance Core.Hashable DeleteMetricFilter

instance Core.NFData DeleteMetricFilter

instance Core.ToHeaders DeleteMetricFilter where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.DeleteMetricFilter" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteMetricFilter where
  toJSON DeleteMetricFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            Core.Just ("filterName" Core..= filterName)
          ]
      )

instance Core.ToPath DeleteMetricFilter where
  toPath = Core.const "/"

instance Core.ToQuery DeleteMetricFilter where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteMetricFilterResponse' smart constructor.
data DeleteMetricFilterResponse = DeleteMetricFilterResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteMetricFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMetricFilterResponse ::
  DeleteMetricFilterResponse
newDeleteMetricFilterResponse =
  DeleteMetricFilterResponse'

instance Core.NFData DeleteMetricFilterResponse
