{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.DeleteTrafficMirrorFilterRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror rule.
module Network.AWS.EC2.DeleteTrafficMirrorFilterRule
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    DeleteTrafficMirrorFilterRule
  where
  type
    Rs DeleteTrafficMirrorFilterRule =
      DeleteTrafficMirrorFilterRuleResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTrafficMirrorFilterRuleResponse'
            Prelude.<$> (x Prelude..@? "trafficMirrorFilterRuleId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteTrafficMirrorFilterRule

instance Prelude.NFData DeleteTrafficMirrorFilterRule

instance
  Prelude.ToHeaders
    DeleteTrafficMirrorFilterRule
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteTrafficMirrorFilterRule where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteTrafficMirrorFilterRule
  where
  toQuery DeleteTrafficMirrorFilterRule' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DeleteTrafficMirrorFilterRule" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "TrafficMirrorFilterRuleId"
          Prelude.=: trafficMirrorFilterRuleId
      ]

-- | /See:/ 'newDeleteTrafficMirrorFilterRuleResponse' smart constructor.
data DeleteTrafficMirrorFilterRuleResponse = DeleteTrafficMirrorFilterRuleResponse'
  { -- | The ID of the deleted Traffic Mirror rule.
    trafficMirrorFilterRuleId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
