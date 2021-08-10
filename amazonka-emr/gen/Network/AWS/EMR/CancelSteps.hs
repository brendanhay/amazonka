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
-- Module      : Network.AWS.EMR.CancelSteps
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a pending step or steps in a running cluster. Available only in
-- Amazon EMR versions 4.8.0 and later, excluding version 5.0.0. A maximum
-- of 256 steps are allowed in each CancelSteps request. CancelSteps is
-- idempotent but asynchronous; it does not guarantee that a step will be
-- canceled, even if the request is successfully submitted. You can only
-- cancel steps that are in a @PENDING@ state.
module Network.AWS.EMR.CancelSteps
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

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
cancelSteps_stepIds = Lens.lens (\CancelSteps' {stepIds} -> stepIds) (\s@CancelSteps' {} a -> s {stepIds = a} :: CancelSteps) Prelude.. Lens._Coerce

instance Core.AWSRequest CancelSteps where
  type AWSResponse CancelSteps = CancelStepsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelStepsResponse'
            Prelude.<$> ( x Core..?> "CancelStepsInfoList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelSteps

instance Prelude.NFData CancelSteps

instance Core.ToHeaders CancelSteps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.CancelSteps" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CancelSteps where
  toJSON CancelSteps' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StepCancellationOption" Core..=)
              Prelude.<$> stepCancellationOption,
            Prelude.Just ("ClusterId" Core..= clusterId),
            Prelude.Just ("StepIds" Core..= stepIds)
          ]
      )

instance Core.ToPath CancelSteps where
  toPath = Prelude.const "/"

instance Core.ToQuery CancelSteps where
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
cancelStepsResponse_cancelStepsInfoList = Lens.lens (\CancelStepsResponse' {cancelStepsInfoList} -> cancelStepsInfoList) (\s@CancelStepsResponse' {} a -> s {cancelStepsInfoList = a} :: CancelStepsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
cancelStepsResponse_httpStatus :: Lens.Lens' CancelStepsResponse Prelude.Int
cancelStepsResponse_httpStatus = Lens.lens (\CancelStepsResponse' {httpStatus} -> httpStatus) (\s@CancelStepsResponse' {} a -> s {httpStatus = a} :: CancelStepsResponse)

instance Prelude.NFData CancelStepsResponse
