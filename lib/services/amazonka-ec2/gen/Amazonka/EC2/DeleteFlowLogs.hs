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
-- Module      : Amazonka.EC2.DeleteFlowLogs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more flow logs.
module Amazonka.EC2.DeleteFlowLogs
  ( -- * Creating a Request
    DeleteFlowLogs (..),
    newDeleteFlowLogs,

    -- * Request Lenses
    deleteFlowLogs_dryRun,
    deleteFlowLogs_flowLogIds,

    -- * Destructuring the Response
    DeleteFlowLogsResponse (..),
    newDeleteFlowLogsResponse,

    -- * Response Lenses
    deleteFlowLogsResponse_unsuccessful,
    deleteFlowLogsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFlowLogs' smart constructor.
data DeleteFlowLogs = DeleteFlowLogs'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more flow log IDs.
    --
    -- Constraint: Maximum of 1000 flow log IDs.
    flowLogIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFlowLogs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteFlowLogs_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'flowLogIds', 'deleteFlowLogs_flowLogIds' - One or more flow log IDs.
--
-- Constraint: Maximum of 1000 flow log IDs.
newDeleteFlowLogs ::
  DeleteFlowLogs
newDeleteFlowLogs =
  DeleteFlowLogs'
    { dryRun = Prelude.Nothing,
      flowLogIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteFlowLogs_dryRun :: Lens.Lens' DeleteFlowLogs (Prelude.Maybe Prelude.Bool)
deleteFlowLogs_dryRun = Lens.lens (\DeleteFlowLogs' {dryRun} -> dryRun) (\s@DeleteFlowLogs' {} a -> s {dryRun = a} :: DeleteFlowLogs)

-- | One or more flow log IDs.
--
-- Constraint: Maximum of 1000 flow log IDs.
deleteFlowLogs_flowLogIds :: Lens.Lens' DeleteFlowLogs [Prelude.Text]
deleteFlowLogs_flowLogIds = Lens.lens (\DeleteFlowLogs' {flowLogIds} -> flowLogIds) (\s@DeleteFlowLogs' {} a -> s {flowLogIds = a} :: DeleteFlowLogs) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteFlowLogs where
  type
    AWSResponse DeleteFlowLogs =
      DeleteFlowLogsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteFlowLogsResponse'
            Prelude.<$> ( x Data..@? "unsuccessful" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFlowLogs where
  hashWithSalt _salt DeleteFlowLogs' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` flowLogIds

instance Prelude.NFData DeleteFlowLogs where
  rnf DeleteFlowLogs' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf flowLogIds

instance Data.ToHeaders DeleteFlowLogs where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteFlowLogs where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteFlowLogs where
  toQuery DeleteFlowLogs' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteFlowLogs" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQueryList "FlowLogId" flowLogIds
      ]

-- | /See:/ 'newDeleteFlowLogsResponse' smart constructor.
data DeleteFlowLogsResponse = DeleteFlowLogsResponse'
  { -- | Information about the flow logs that could not be deleted successfully.
    unsuccessful :: Prelude.Maybe [UnsuccessfulItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFlowLogsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unsuccessful', 'deleteFlowLogsResponse_unsuccessful' - Information about the flow logs that could not be deleted successfully.
--
-- 'httpStatus', 'deleteFlowLogsResponse_httpStatus' - The response's http status code.
newDeleteFlowLogsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFlowLogsResponse
newDeleteFlowLogsResponse pHttpStatus_ =
  DeleteFlowLogsResponse'
    { unsuccessful =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the flow logs that could not be deleted successfully.
deleteFlowLogsResponse_unsuccessful :: Lens.Lens' DeleteFlowLogsResponse (Prelude.Maybe [UnsuccessfulItem])
deleteFlowLogsResponse_unsuccessful = Lens.lens (\DeleteFlowLogsResponse' {unsuccessful} -> unsuccessful) (\s@DeleteFlowLogsResponse' {} a -> s {unsuccessful = a} :: DeleteFlowLogsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteFlowLogsResponse_httpStatus :: Lens.Lens' DeleteFlowLogsResponse Prelude.Int
deleteFlowLogsResponse_httpStatus = Lens.lens (\DeleteFlowLogsResponse' {httpStatus} -> httpStatus) (\s@DeleteFlowLogsResponse' {} a -> s {httpStatus = a} :: DeleteFlowLogsResponse)

instance Prelude.NFData DeleteFlowLogsResponse where
  rnf DeleteFlowLogsResponse' {..} =
    Prelude.rnf unsuccessful
      `Prelude.seq` Prelude.rnf httpStatus
