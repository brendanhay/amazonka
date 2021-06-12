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
-- Module      : Network.AWS.Redshift.Types.ScheduledActionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduledActionType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.PauseClusterMessage
import Network.AWS.Redshift.Types.ResizeClusterMessage
import Network.AWS.Redshift.Types.ResumeClusterMessage

-- | The action type that specifies an Amazon Redshift API operation that is
-- supported by the Amazon Redshift scheduler.
--
-- /See:/ 'newScheduledActionType' smart constructor.
data ScheduledActionType = ScheduledActionType'
  { -- | An action that runs a @ResumeCluster@ API operation.
    resumeCluster :: Core.Maybe ResumeClusterMessage,
    -- | An action that runs a @ResizeCluster@ API operation.
    resizeCluster :: Core.Maybe ResizeClusterMessage,
    -- | An action that runs a @PauseCluster@ API operation.
    pauseCluster :: Core.Maybe PauseClusterMessage
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScheduledActionType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resumeCluster', 'scheduledActionType_resumeCluster' - An action that runs a @ResumeCluster@ API operation.
--
-- 'resizeCluster', 'scheduledActionType_resizeCluster' - An action that runs a @ResizeCluster@ API operation.
--
-- 'pauseCluster', 'scheduledActionType_pauseCluster' - An action that runs a @PauseCluster@ API operation.
newScheduledActionType ::
  ScheduledActionType
newScheduledActionType =
  ScheduledActionType'
    { resumeCluster = Core.Nothing,
      resizeCluster = Core.Nothing,
      pauseCluster = Core.Nothing
    }

-- | An action that runs a @ResumeCluster@ API operation.
scheduledActionType_resumeCluster :: Lens.Lens' ScheduledActionType (Core.Maybe ResumeClusterMessage)
scheduledActionType_resumeCluster = Lens.lens (\ScheduledActionType' {resumeCluster} -> resumeCluster) (\s@ScheduledActionType' {} a -> s {resumeCluster = a} :: ScheduledActionType)

-- | An action that runs a @ResizeCluster@ API operation.
scheduledActionType_resizeCluster :: Lens.Lens' ScheduledActionType (Core.Maybe ResizeClusterMessage)
scheduledActionType_resizeCluster = Lens.lens (\ScheduledActionType' {resizeCluster} -> resizeCluster) (\s@ScheduledActionType' {} a -> s {resizeCluster = a} :: ScheduledActionType)

-- | An action that runs a @PauseCluster@ API operation.
scheduledActionType_pauseCluster :: Lens.Lens' ScheduledActionType (Core.Maybe PauseClusterMessage)
scheduledActionType_pauseCluster = Lens.lens (\ScheduledActionType' {pauseCluster} -> pauseCluster) (\s@ScheduledActionType' {} a -> s {pauseCluster = a} :: ScheduledActionType)

instance Core.FromXML ScheduledActionType where
  parseXML x =
    ScheduledActionType'
      Core.<$> (x Core..@? "ResumeCluster")
      Core.<*> (x Core..@? "ResizeCluster")
      Core.<*> (x Core..@? "PauseCluster")

instance Core.Hashable ScheduledActionType

instance Core.NFData ScheduledActionType

instance Core.ToQuery ScheduledActionType where
  toQuery ScheduledActionType' {..} =
    Core.mconcat
      [ "ResumeCluster" Core.=: resumeCluster,
        "ResizeCluster" Core.=: resizeCluster,
        "PauseCluster" Core.=: pauseCluster
      ]
