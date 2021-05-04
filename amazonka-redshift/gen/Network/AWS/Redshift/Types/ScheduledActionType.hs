{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    resumeCluster :: Prelude.Maybe ResumeClusterMessage,
    -- | An action that runs a @ResizeCluster@ API operation.
    resizeCluster :: Prelude.Maybe ResizeClusterMessage,
    -- | An action that runs a @PauseCluster@ API operation.
    pauseCluster :: Prelude.Maybe PauseClusterMessage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { resumeCluster =
        Prelude.Nothing,
      resizeCluster = Prelude.Nothing,
      pauseCluster = Prelude.Nothing
    }

-- | An action that runs a @ResumeCluster@ API operation.
scheduledActionType_resumeCluster :: Lens.Lens' ScheduledActionType (Prelude.Maybe ResumeClusterMessage)
scheduledActionType_resumeCluster = Lens.lens (\ScheduledActionType' {resumeCluster} -> resumeCluster) (\s@ScheduledActionType' {} a -> s {resumeCluster = a} :: ScheduledActionType)

-- | An action that runs a @ResizeCluster@ API operation.
scheduledActionType_resizeCluster :: Lens.Lens' ScheduledActionType (Prelude.Maybe ResizeClusterMessage)
scheduledActionType_resizeCluster = Lens.lens (\ScheduledActionType' {resizeCluster} -> resizeCluster) (\s@ScheduledActionType' {} a -> s {resizeCluster = a} :: ScheduledActionType)

-- | An action that runs a @PauseCluster@ API operation.
scheduledActionType_pauseCluster :: Lens.Lens' ScheduledActionType (Prelude.Maybe PauseClusterMessage)
scheduledActionType_pauseCluster = Lens.lens (\ScheduledActionType' {pauseCluster} -> pauseCluster) (\s@ScheduledActionType' {} a -> s {pauseCluster = a} :: ScheduledActionType)

instance Prelude.FromXML ScheduledActionType where
  parseXML x =
    ScheduledActionType'
      Prelude.<$> (x Prelude..@? "ResumeCluster")
      Prelude.<*> (x Prelude..@? "ResizeCluster")
      Prelude.<*> (x Prelude..@? "PauseCluster")

instance Prelude.Hashable ScheduledActionType

instance Prelude.NFData ScheduledActionType

instance Prelude.ToQuery ScheduledActionType where
  toQuery ScheduledActionType' {..} =
    Prelude.mconcat
      [ "ResumeCluster" Prelude.=: resumeCluster,
        "ResizeCluster" Prelude.=: resizeCluster,
        "PauseCluster" Prelude.=: pauseCluster
      ]
