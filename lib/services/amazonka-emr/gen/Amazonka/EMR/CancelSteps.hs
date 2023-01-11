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
-- Module      : Amazonka.EMR.CancelSteps
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a pending step or steps in a running cluster. Available only in
-- Amazon EMR versions 4.8.0 and later, excluding version 5.0.0. A maximum
-- of 256 steps are allowed in each CancelSteps request. CancelSteps is
-- idempotent but asynchronous; it does not guarantee that a step will be
-- canceled, even if the request is successfully submitted. When you use
-- Amazon EMR versions 5.28.0 and later, you can cancel steps that are in a
-- @PENDING@ or @RUNNING@ state. In earlier versions of Amazon EMR, you can
-- only cancel steps that are in a @PENDING@ state.
module Amazonka.EMR.CancelSteps
  ( -- * Creating a Request
    CancelSteps (..),
    newCancelSteps,

    -- * Request Lenses
    cancelSteps_stepCancellationOption,
    cancelSteps_clusterId,
    cancelSteps_stepIds,

    -- * Destructuring the Response
    CancelStepsResponse (..),
    newCancelStepsResponse,

    -- * Response Lenses
    cancelStepsResponse_cancelStepsInfoList,
    cancelStepsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input argument to the CancelSteps operation.
--
-- /See:/ 'newCancelSteps' smart constructor.
data CancelSteps = CancelSteps'
  { -- | The option to choose to cancel @RUNNING@ steps. By default, the value is
    -- @SEND_INTERRUPT@.
    stepCancellationOption :: Prelude.Maybe StepCancellationOption,
    -- | The @ClusterID@ for the specified steps that will be canceled. Use
    -- RunJobFlow and ListClusters to get ClusterIDs.
    clusterId :: Prelude.Text,
    -- | The list of @StepIDs@ to cancel. Use ListSteps to get steps and their
    -- states for the specified cluster.
    stepIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelSteps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stepCancellationOption', 'cancelSteps_stepCancellationOption' - The option to choose to cancel @RUNNING@ steps. By default, the value is
-- @SEND_INTERRUPT@.
--
-- 'clusterId', 'cancelSteps_clusterId' - The @ClusterID@ for the specified steps that will be canceled. Use
-- RunJobFlow and ListClusters to get ClusterIDs.
--
-- 'stepIds', 'cancelSteps_stepIds' - The list of @StepIDs@ to cancel. Use ListSteps to get steps and their
-- states for the specified cluster.
newCancelSteps ::
  -- | 'clusterId'
  Prelude.Text ->
  CancelSteps
newCancelSteps pClusterId_ =
  CancelSteps'
    { stepCancellationOption =
        Prelude.Nothing,
      clusterId = pClusterId_,
      stepIds = Prelude.mempty
    }

-- | The option to choose to cancel @RUNNING@ steps. By default, the value is
-- @SEND_INTERRUPT@.
cancelSteps_stepCancellationOption :: Lens.Lens' CancelSteps (Prelude.Maybe StepCancellationOption)
cancelSteps_stepCancellationOption = Lens.lens (\CancelSteps' {stepCancellationOption} -> stepCancellationOption) (\s@CancelSteps' {} a -> s {stepCancellationOption = a} :: CancelSteps)

-- | The @ClusterID@ for the specified steps that will be canceled. Use
-- RunJobFlow and ListClusters to get ClusterIDs.
cancelSteps_clusterId :: Lens.Lens' CancelSteps Prelude.Text
cancelSteps_clusterId = Lens.lens (\CancelSteps' {clusterId} -> clusterId) (\s@CancelSteps' {} a -> s {clusterId = a} :: CancelSteps)

-- | The list of @StepIDs@ to cancel. Use ListSteps to get steps and their
-- states for the specified cluster.
cancelSteps_stepIds :: Lens.Lens' CancelSteps [Prelude.Text]
cancelSteps_stepIds = Lens.lens (\CancelSteps' {stepIds} -> stepIds) (\s@CancelSteps' {} a -> s {stepIds = a} :: CancelSteps) Prelude.. Lens.coerced

instance Core.AWSRequest CancelSteps where
  type AWSResponse CancelSteps = CancelStepsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelStepsResponse'
            Prelude.<$> ( x Data..?> "CancelStepsInfoList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelSteps where
  hashWithSalt _salt CancelSteps' {..} =
    _salt `Prelude.hashWithSalt` stepCancellationOption
      `Prelude.hashWithSalt` clusterId
      `Prelude.hashWithSalt` stepIds

instance Prelude.NFData CancelSteps where
  rnf CancelSteps' {..} =
    Prelude.rnf stepCancellationOption
      `Prelude.seq` Prelude.rnf clusterId
      `Prelude.seq` Prelude.rnf stepIds

instance Data.ToHeaders CancelSteps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.CancelSteps" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelSteps where
  toJSON CancelSteps' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StepCancellationOption" Data..=)
              Prelude.<$> stepCancellationOption,
            Prelude.Just ("ClusterId" Data..= clusterId),
            Prelude.Just ("StepIds" Data..= stepIds)
          ]
      )

instance Data.ToPath CancelSteps where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelSteps where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the CancelSteps operation.
--
-- /See:/ 'newCancelStepsResponse' smart constructor.
data CancelStepsResponse = CancelStepsResponse'
  { -- | A list of CancelStepsInfo, which shows the status of specified cancel
    -- requests for each @StepID@ specified.
    cancelStepsInfoList :: Prelude.Maybe [CancelStepsInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelStepsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cancelStepsInfoList', 'cancelStepsResponse_cancelStepsInfoList' - A list of CancelStepsInfo, which shows the status of specified cancel
-- requests for each @StepID@ specified.
--
-- 'httpStatus', 'cancelStepsResponse_httpStatus' - The response's http status code.
newCancelStepsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelStepsResponse
newCancelStepsResponse pHttpStatus_ =
  CancelStepsResponse'
    { cancelStepsInfoList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of CancelStepsInfo, which shows the status of specified cancel
-- requests for each @StepID@ specified.
cancelStepsResponse_cancelStepsInfoList :: Lens.Lens' CancelStepsResponse (Prelude.Maybe [CancelStepsInfo])
cancelStepsResponse_cancelStepsInfoList = Lens.lens (\CancelStepsResponse' {cancelStepsInfoList} -> cancelStepsInfoList) (\s@CancelStepsResponse' {} a -> s {cancelStepsInfoList = a} :: CancelStepsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
cancelStepsResponse_httpStatus :: Lens.Lens' CancelStepsResponse Prelude.Int
cancelStepsResponse_httpStatus = Lens.lens (\CancelStepsResponse' {httpStatus} -> httpStatus) (\s@CancelStepsResponse' {} a -> s {httpStatus = a} :: CancelStepsResponse)

instance Prelude.NFData CancelStepsResponse where
  rnf CancelStepsResponse' {..} =
    Prelude.rnf cancelStepsInfoList
      `Prelude.seq` Prelude.rnf httpStatus
