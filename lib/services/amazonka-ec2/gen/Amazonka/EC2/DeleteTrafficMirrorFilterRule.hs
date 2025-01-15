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
-- Module      : Amazonka.EC2.DeleteTrafficMirrorFilterRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror rule.
module Amazonka.EC2.DeleteTrafficMirrorFilterRule
  ( -- * Creating a Request
    DeleteTrafficMirrorFilterRule (..),
    newDeleteTrafficMirrorFilterRule,

    -- * Request Lenses
    deleteTrafficMirrorFilterRule_dryRun,
    deleteTrafficMirrorFilterRule_trafficMirrorFilterRuleId,

    -- * Destructuring the Response
    DeleteTrafficMirrorFilterRuleResponse (..),
    newDeleteTrafficMirrorFilterRuleResponse,

    -- * Response Lenses
    deleteTrafficMirrorFilterRuleResponse_trafficMirrorFilterRuleId,
    deleteTrafficMirrorFilterRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTrafficMirrorFilterRule' smart constructor.
data DeleteTrafficMirrorFilterRule = DeleteTrafficMirrorFilterRule'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Traffic Mirror rule.
    trafficMirrorFilterRuleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrafficMirrorFilterRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteTrafficMirrorFilterRule_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'trafficMirrorFilterRuleId', 'deleteTrafficMirrorFilterRule_trafficMirrorFilterRuleId' - The ID of the Traffic Mirror rule.
newDeleteTrafficMirrorFilterRule ::
  -- | 'trafficMirrorFilterRuleId'
  Prelude.Text ->
  DeleteTrafficMirrorFilterRule
newDeleteTrafficMirrorFilterRule
  pTrafficMirrorFilterRuleId_ =
    DeleteTrafficMirrorFilterRule'
      { dryRun =
          Prelude.Nothing,
        trafficMirrorFilterRuleId =
          pTrafficMirrorFilterRuleId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTrafficMirrorFilterRule_dryRun :: Lens.Lens' DeleteTrafficMirrorFilterRule (Prelude.Maybe Prelude.Bool)
deleteTrafficMirrorFilterRule_dryRun = Lens.lens (\DeleteTrafficMirrorFilterRule' {dryRun} -> dryRun) (\s@DeleteTrafficMirrorFilterRule' {} a -> s {dryRun = a} :: DeleteTrafficMirrorFilterRule)

-- | The ID of the Traffic Mirror rule.
deleteTrafficMirrorFilterRule_trafficMirrorFilterRuleId :: Lens.Lens' DeleteTrafficMirrorFilterRule Prelude.Text
deleteTrafficMirrorFilterRule_trafficMirrorFilterRuleId = Lens.lens (\DeleteTrafficMirrorFilterRule' {trafficMirrorFilterRuleId} -> trafficMirrorFilterRuleId) (\s@DeleteTrafficMirrorFilterRule' {} a -> s {trafficMirrorFilterRuleId = a} :: DeleteTrafficMirrorFilterRule)

instance
  Core.AWSRequest
    DeleteTrafficMirrorFilterRule
  where
  type
    AWSResponse DeleteTrafficMirrorFilterRule =
      DeleteTrafficMirrorFilterRuleResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTrafficMirrorFilterRuleResponse'
            Prelude.<$> (x Data..@? "trafficMirrorFilterRuleId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteTrafficMirrorFilterRule
  where
  hashWithSalt _salt DeleteTrafficMirrorFilterRule' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` trafficMirrorFilterRuleId

instance Prelude.NFData DeleteTrafficMirrorFilterRule where
  rnf DeleteTrafficMirrorFilterRule' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf trafficMirrorFilterRuleId

instance Data.ToHeaders DeleteTrafficMirrorFilterRule where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteTrafficMirrorFilterRule where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTrafficMirrorFilterRule where
  toQuery DeleteTrafficMirrorFilterRule' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteTrafficMirrorFilterRule" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "TrafficMirrorFilterRuleId"
          Data.=: trafficMirrorFilterRuleId
      ]

-- | /See:/ 'newDeleteTrafficMirrorFilterRuleResponse' smart constructor.
data DeleteTrafficMirrorFilterRuleResponse = DeleteTrafficMirrorFilterRuleResponse'
  { -- | The ID of the deleted Traffic Mirror rule.
    trafficMirrorFilterRuleId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrafficMirrorFilterRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficMirrorFilterRuleId', 'deleteTrafficMirrorFilterRuleResponse_trafficMirrorFilterRuleId' - The ID of the deleted Traffic Mirror rule.
--
-- 'httpStatus', 'deleteTrafficMirrorFilterRuleResponse_httpStatus' - The response's http status code.
newDeleteTrafficMirrorFilterRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTrafficMirrorFilterRuleResponse
newDeleteTrafficMirrorFilterRuleResponse pHttpStatus_ =
  DeleteTrafficMirrorFilterRuleResponse'
    { trafficMirrorFilterRuleId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the deleted Traffic Mirror rule.
deleteTrafficMirrorFilterRuleResponse_trafficMirrorFilterRuleId :: Lens.Lens' DeleteTrafficMirrorFilterRuleResponse (Prelude.Maybe Prelude.Text)
deleteTrafficMirrorFilterRuleResponse_trafficMirrorFilterRuleId = Lens.lens (\DeleteTrafficMirrorFilterRuleResponse' {trafficMirrorFilterRuleId} -> trafficMirrorFilterRuleId) (\s@DeleteTrafficMirrorFilterRuleResponse' {} a -> s {trafficMirrorFilterRuleId = a} :: DeleteTrafficMirrorFilterRuleResponse)

-- | The response's http status code.
deleteTrafficMirrorFilterRuleResponse_httpStatus :: Lens.Lens' DeleteTrafficMirrorFilterRuleResponse Prelude.Int
deleteTrafficMirrorFilterRuleResponse_httpStatus = Lens.lens (\DeleteTrafficMirrorFilterRuleResponse' {httpStatus} -> httpStatus) (\s@DeleteTrafficMirrorFilterRuleResponse' {} a -> s {httpStatus = a} :: DeleteTrafficMirrorFilterRuleResponse)

instance
  Prelude.NFData
    DeleteTrafficMirrorFilterRuleResponse
  where
  rnf DeleteTrafficMirrorFilterRuleResponse' {..} =
    Prelude.rnf trafficMirrorFilterRuleId `Prelude.seq`
      Prelude.rnf httpStatus
