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
-- Module      : Amazonka.Backup.DeleteReportPlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the report plan specified by a report plan name.
module Amazonka.Backup.DeleteReportPlan
  ( -- * Creating a Request
    DeleteReportPlan (..),
    newDeleteReportPlan,

    -- * Request Lenses
    deleteReportPlan_reportPlanName,

    -- * Destructuring the Response
    DeleteReportPlanResponse (..),
    newDeleteReportPlanResponse,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteReportPlan' smart constructor.
data DeleteReportPlan = DeleteReportPlan'
  { -- | The unique name of a report plan.
    reportPlanName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReportPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportPlanName', 'deleteReportPlan_reportPlanName' - The unique name of a report plan.
newDeleteReportPlan ::
  -- | 'reportPlanName'
  Prelude.Text ->
  DeleteReportPlan
newDeleteReportPlan pReportPlanName_ =
  DeleteReportPlan'
    { reportPlanName =
        pReportPlanName_
    }

-- | The unique name of a report plan.
deleteReportPlan_reportPlanName :: Lens.Lens' DeleteReportPlan Prelude.Text
deleteReportPlan_reportPlanName = Lens.lens (\DeleteReportPlan' {reportPlanName} -> reportPlanName) (\s@DeleteReportPlan' {} a -> s {reportPlanName = a} :: DeleteReportPlan)

instance Core.AWSRequest DeleteReportPlan where
  type
    AWSResponse DeleteReportPlan =
      DeleteReportPlanResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteReportPlanResponse'

instance Prelude.Hashable DeleteReportPlan where
  hashWithSalt _salt DeleteReportPlan' {..} =
    _salt `Prelude.hashWithSalt` reportPlanName

instance Prelude.NFData DeleteReportPlan where
  rnf DeleteReportPlan' {..} =
    Prelude.rnf reportPlanName

instance Data.ToHeaders DeleteReportPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteReportPlan where
  toPath DeleteReportPlan' {..} =
    Prelude.mconcat
      ["/audit/report-plans/", Data.toBS reportPlanName]

instance Data.ToQuery DeleteReportPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteReportPlanResponse' smart constructor.
data DeleteReportPlanResponse = DeleteReportPlanResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReportPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteReportPlanResponse ::
  DeleteReportPlanResponse
newDeleteReportPlanResponse =
  DeleteReportPlanResponse'

instance Prelude.NFData DeleteReportPlanResponse where
  rnf _ = ()
