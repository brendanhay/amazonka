{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Batch.Types.EksAttemptDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksAttemptDetail where

import Amazonka.Batch.Types.EksAttemptContainerDetail
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the details of a job attempt for a job attempt
-- by an Amazon EKS container.
--
-- /See:/ 'newEksAttemptDetail' smart constructor.
data EksAttemptDetail = EksAttemptDetail'
  { -- | The details for the final status of the containers for this job attempt.
    containers :: Prelude.Maybe [EksAttemptContainerDetail],
    -- | The name of the node for this job attempt.
    nodeName :: Prelude.Maybe Prelude.Text,
    -- | The name of the pod for this job attempt.
    podName :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp (in milliseconds) for when the attempt was started
    -- (when the attempt transitioned from the @STARTING@ state to the
    -- @RUNNING@ state).
    startedAt :: Prelude.Maybe Prelude.Integer,
    -- | A short, human-readable string to provide additional details for the
    -- current status of the job attempt.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp (in milliseconds) for when the attempt was stopped.
    -- This happens when the attempt transitioned from the @RUNNING@ state to a
    -- terminal state, such as @SUCCEEDED@ or @FAILED@.
    stoppedAt :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksAttemptDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containers', 'eksAttemptDetail_containers' - The details for the final status of the containers for this job attempt.
--
-- 'nodeName', 'eksAttemptDetail_nodeName' - The name of the node for this job attempt.
--
-- 'podName', 'eksAttemptDetail_podName' - The name of the pod for this job attempt.
--
-- 'startedAt', 'eksAttemptDetail_startedAt' - The Unix timestamp (in milliseconds) for when the attempt was started
-- (when the attempt transitioned from the @STARTING@ state to the
-- @RUNNING@ state).
--
-- 'statusReason', 'eksAttemptDetail_statusReason' - A short, human-readable string to provide additional details for the
-- current status of the job attempt.
--
-- 'stoppedAt', 'eksAttemptDetail_stoppedAt' - The Unix timestamp (in milliseconds) for when the attempt was stopped.
-- This happens when the attempt transitioned from the @RUNNING@ state to a
-- terminal state, such as @SUCCEEDED@ or @FAILED@.
newEksAttemptDetail ::
  EksAttemptDetail
newEksAttemptDetail =
  EksAttemptDetail'
    { containers = Prelude.Nothing,
      nodeName = Prelude.Nothing,
      podName = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      stoppedAt = Prelude.Nothing
    }

-- | The details for the final status of the containers for this job attempt.
eksAttemptDetail_containers :: Lens.Lens' EksAttemptDetail (Prelude.Maybe [EksAttemptContainerDetail])
eksAttemptDetail_containers = Lens.lens (\EksAttemptDetail' {containers} -> containers) (\s@EksAttemptDetail' {} a -> s {containers = a} :: EksAttemptDetail) Prelude.. Lens.mapping Lens.coerced

-- | The name of the node for this job attempt.
eksAttemptDetail_nodeName :: Lens.Lens' EksAttemptDetail (Prelude.Maybe Prelude.Text)
eksAttemptDetail_nodeName = Lens.lens (\EksAttemptDetail' {nodeName} -> nodeName) (\s@EksAttemptDetail' {} a -> s {nodeName = a} :: EksAttemptDetail)

-- | The name of the pod for this job attempt.
eksAttemptDetail_podName :: Lens.Lens' EksAttemptDetail (Prelude.Maybe Prelude.Text)
eksAttemptDetail_podName = Lens.lens (\EksAttemptDetail' {podName} -> podName) (\s@EksAttemptDetail' {} a -> s {podName = a} :: EksAttemptDetail)

-- | The Unix timestamp (in milliseconds) for when the attempt was started
-- (when the attempt transitioned from the @STARTING@ state to the
-- @RUNNING@ state).
eksAttemptDetail_startedAt :: Lens.Lens' EksAttemptDetail (Prelude.Maybe Prelude.Integer)
eksAttemptDetail_startedAt = Lens.lens (\EksAttemptDetail' {startedAt} -> startedAt) (\s@EksAttemptDetail' {} a -> s {startedAt = a} :: EksAttemptDetail)

-- | A short, human-readable string to provide additional details for the
-- current status of the job attempt.
eksAttemptDetail_statusReason :: Lens.Lens' EksAttemptDetail (Prelude.Maybe Prelude.Text)
eksAttemptDetail_statusReason = Lens.lens (\EksAttemptDetail' {statusReason} -> statusReason) (\s@EksAttemptDetail' {} a -> s {statusReason = a} :: EksAttemptDetail)

-- | The Unix timestamp (in milliseconds) for when the attempt was stopped.
-- This happens when the attempt transitioned from the @RUNNING@ state to a
-- terminal state, such as @SUCCEEDED@ or @FAILED@.
eksAttemptDetail_stoppedAt :: Lens.Lens' EksAttemptDetail (Prelude.Maybe Prelude.Integer)
eksAttemptDetail_stoppedAt = Lens.lens (\EksAttemptDetail' {stoppedAt} -> stoppedAt) (\s@EksAttemptDetail' {} a -> s {stoppedAt = a} :: EksAttemptDetail)

instance Data.FromJSON EksAttemptDetail where
  parseJSON =
    Data.withObject
      "EksAttemptDetail"
      ( \x ->
          EksAttemptDetail'
            Prelude.<$> (x Data..:? "containers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "nodeName")
            Prelude.<*> (x Data..:? "podName")
            Prelude.<*> (x Data..:? "startedAt")
            Prelude.<*> (x Data..:? "statusReason")
            Prelude.<*> (x Data..:? "stoppedAt")
      )

instance Prelude.Hashable EksAttemptDetail where
  hashWithSalt _salt EksAttemptDetail' {..} =
    _salt `Prelude.hashWithSalt` containers
      `Prelude.hashWithSalt` nodeName
      `Prelude.hashWithSalt` podName
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` stoppedAt

instance Prelude.NFData EksAttemptDetail where
  rnf EksAttemptDetail' {..} =
    Prelude.rnf containers
      `Prelude.seq` Prelude.rnf nodeName
      `Prelude.seq` Prelude.rnf podName
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf stoppedAt
