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
-- Module      : Amazonka.AutoScaling.BatchPutScheduledUpdateGroupAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates one or more scheduled scaling actions for an Auto
-- Scaling group.
module Amazonka.AutoScaling.BatchPutScheduledUpdateGroupAction
  ( -- * Creating a Request
    BatchPutScheduledUpdateGroupAction (..),
    newBatchPutScheduledUpdateGroupAction,

    -- * Request Lenses
    batchPutScheduledUpdateGroupAction_autoScalingGroupName,
    batchPutScheduledUpdateGroupAction_scheduledUpdateGroupActions,

    -- * Destructuring the Response
    BatchPutScheduledUpdateGroupActionResponse (..),
    newBatchPutScheduledUpdateGroupActionResponse,

    -- * Response Lenses
    batchPutScheduledUpdateGroupActionResponse_failedScheduledUpdateGroupActions,
    batchPutScheduledUpdateGroupActionResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchPutScheduledUpdateGroupAction' smart constructor.
data BatchPutScheduledUpdateGroupAction = BatchPutScheduledUpdateGroupAction'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | One or more scheduled actions. The maximum number allowed is 50.
    scheduledUpdateGroupActions :: [ScheduledUpdateGroupActionRequest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutScheduledUpdateGroupAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupName', 'batchPutScheduledUpdateGroupAction_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'scheduledUpdateGroupActions', 'batchPutScheduledUpdateGroupAction_scheduledUpdateGroupActions' - One or more scheduled actions. The maximum number allowed is 50.
newBatchPutScheduledUpdateGroupAction ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  BatchPutScheduledUpdateGroupAction
newBatchPutScheduledUpdateGroupAction
  pAutoScalingGroupName_ =
    BatchPutScheduledUpdateGroupAction'
      { autoScalingGroupName =
          pAutoScalingGroupName_,
        scheduledUpdateGroupActions =
          Prelude.mempty
      }

-- | The name of the Auto Scaling group.
batchPutScheduledUpdateGroupAction_autoScalingGroupName :: Lens.Lens' BatchPutScheduledUpdateGroupAction Prelude.Text
batchPutScheduledUpdateGroupAction_autoScalingGroupName = Lens.lens (\BatchPutScheduledUpdateGroupAction' {autoScalingGroupName} -> autoScalingGroupName) (\s@BatchPutScheduledUpdateGroupAction' {} a -> s {autoScalingGroupName = a} :: BatchPutScheduledUpdateGroupAction)

-- | One or more scheduled actions. The maximum number allowed is 50.
batchPutScheduledUpdateGroupAction_scheduledUpdateGroupActions :: Lens.Lens' BatchPutScheduledUpdateGroupAction [ScheduledUpdateGroupActionRequest]
batchPutScheduledUpdateGroupAction_scheduledUpdateGroupActions = Lens.lens (\BatchPutScheduledUpdateGroupAction' {scheduledUpdateGroupActions} -> scheduledUpdateGroupActions) (\s@BatchPutScheduledUpdateGroupAction' {} a -> s {scheduledUpdateGroupActions = a} :: BatchPutScheduledUpdateGroupAction) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchPutScheduledUpdateGroupAction
  where
  type
    AWSResponse BatchPutScheduledUpdateGroupAction =
      BatchPutScheduledUpdateGroupActionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "BatchPutScheduledUpdateGroupActionResult"
      ( \s h x ->
          BatchPutScheduledUpdateGroupActionResponse'
            Prelude.<$> ( x
                            Data..@? "FailedScheduledUpdateGroupActions"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchPutScheduledUpdateGroupAction
  where
  hashWithSalt
    _salt
    BatchPutScheduledUpdateGroupAction' {..} =
      _salt
        `Prelude.hashWithSalt` autoScalingGroupName
        `Prelude.hashWithSalt` scheduledUpdateGroupActions

instance
  Prelude.NFData
    BatchPutScheduledUpdateGroupAction
  where
  rnf BatchPutScheduledUpdateGroupAction' {..} =
    Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf scheduledUpdateGroupActions

instance
  Data.ToHeaders
    BatchPutScheduledUpdateGroupAction
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    BatchPutScheduledUpdateGroupAction
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    BatchPutScheduledUpdateGroupAction
  where
  toQuery BatchPutScheduledUpdateGroupAction' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "BatchPutScheduledUpdateGroupAction" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "AutoScalingGroupName" Data.=: autoScalingGroupName,
        "ScheduledUpdateGroupActions"
          Data.=: Data.toQueryList
            "member"
            scheduledUpdateGroupActions
      ]

-- | /See:/ 'newBatchPutScheduledUpdateGroupActionResponse' smart constructor.
data BatchPutScheduledUpdateGroupActionResponse = BatchPutScheduledUpdateGroupActionResponse'
  { -- | The names of the scheduled actions that could not be created or updated,
    -- including an error message.
    failedScheduledUpdateGroupActions :: Prelude.Maybe [FailedScheduledUpdateGroupActionRequest],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutScheduledUpdateGroupActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedScheduledUpdateGroupActions', 'batchPutScheduledUpdateGroupActionResponse_failedScheduledUpdateGroupActions' - The names of the scheduled actions that could not be created or updated,
-- including an error message.
--
-- 'httpStatus', 'batchPutScheduledUpdateGroupActionResponse_httpStatus' - The response's http status code.
newBatchPutScheduledUpdateGroupActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchPutScheduledUpdateGroupActionResponse
newBatchPutScheduledUpdateGroupActionResponse
  pHttpStatus_ =
    BatchPutScheduledUpdateGroupActionResponse'
      { failedScheduledUpdateGroupActions =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The names of the scheduled actions that could not be created or updated,
-- including an error message.
batchPutScheduledUpdateGroupActionResponse_failedScheduledUpdateGroupActions :: Lens.Lens' BatchPutScheduledUpdateGroupActionResponse (Prelude.Maybe [FailedScheduledUpdateGroupActionRequest])
batchPutScheduledUpdateGroupActionResponse_failedScheduledUpdateGroupActions = Lens.lens (\BatchPutScheduledUpdateGroupActionResponse' {failedScheduledUpdateGroupActions} -> failedScheduledUpdateGroupActions) (\s@BatchPutScheduledUpdateGroupActionResponse' {} a -> s {failedScheduledUpdateGroupActions = a} :: BatchPutScheduledUpdateGroupActionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchPutScheduledUpdateGroupActionResponse_httpStatus :: Lens.Lens' BatchPutScheduledUpdateGroupActionResponse Prelude.Int
batchPutScheduledUpdateGroupActionResponse_httpStatus = Lens.lens (\BatchPutScheduledUpdateGroupActionResponse' {httpStatus} -> httpStatus) (\s@BatchPutScheduledUpdateGroupActionResponse' {} a -> s {httpStatus = a} :: BatchPutScheduledUpdateGroupActionResponse)

instance
  Prelude.NFData
    BatchPutScheduledUpdateGroupActionResponse
  where
  rnf BatchPutScheduledUpdateGroupActionResponse' {..} =
    Prelude.rnf failedScheduledUpdateGroupActions
      `Prelude.seq` Prelude.rnf httpStatus
