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
-- Module      : Amazonka.CloudWatchLogs.DeleteMetricFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified metric filter.
module Amazonka.CloudWatchLogs.DeleteMetricFilter
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

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMetricFilter' smart constructor.
data DeleteMetricFilter = DeleteMetricFilter'
  { -- | The name of the log group.
    logGroupName :: Prelude.Text,
    -- | The name of the metric filter.
    filterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'filterName'
  Prelude.Text ->
  DeleteMetricFilter
newDeleteMetricFilter pLogGroupName_ pFilterName_ =
  DeleteMetricFilter'
    { logGroupName = pLogGroupName_,
      filterName = pFilterName_
    }

-- | The name of the log group.
deleteMetricFilter_logGroupName :: Lens.Lens' DeleteMetricFilter Prelude.Text
deleteMetricFilter_logGroupName = Lens.lens (\DeleteMetricFilter' {logGroupName} -> logGroupName) (\s@DeleteMetricFilter' {} a -> s {logGroupName = a} :: DeleteMetricFilter)

-- | The name of the metric filter.
deleteMetricFilter_filterName :: Lens.Lens' DeleteMetricFilter Prelude.Text
deleteMetricFilter_filterName = Lens.lens (\DeleteMetricFilter' {filterName} -> filterName) (\s@DeleteMetricFilter' {} a -> s {filterName = a} :: DeleteMetricFilter)

instance Core.AWSRequest DeleteMetricFilter where
  type
    AWSResponse DeleteMetricFilter =
      DeleteMetricFilterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteMetricFilterResponse'

instance Prelude.Hashable DeleteMetricFilter where
  hashWithSalt _salt DeleteMetricFilter' {..} =
    _salt `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` filterName

instance Prelude.NFData DeleteMetricFilter where
  rnf DeleteMetricFilter' {..} =
    Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf filterName

instance Data.ToHeaders DeleteMetricFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.DeleteMetricFilter" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteMetricFilter where
  toJSON DeleteMetricFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("logGroupName" Data..= logGroupName),
            Prelude.Just ("filterName" Data..= filterName)
          ]
      )

instance Data.ToPath DeleteMetricFilter where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteMetricFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMetricFilterResponse' smart constructor.
data DeleteMetricFilterResponse = DeleteMetricFilterResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMetricFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMetricFilterResponse ::
  DeleteMetricFilterResponse
newDeleteMetricFilterResponse =
  DeleteMetricFilterResponse'

instance Prelude.NFData DeleteMetricFilterResponse where
  rnf _ = ()
