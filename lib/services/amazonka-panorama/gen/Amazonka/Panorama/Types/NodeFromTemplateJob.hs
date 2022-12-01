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
-- Module      : Amazonka.Panorama.Types.NodeFromTemplateJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.NodeFromTemplateJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Panorama.Types.NodeFromTemplateJobStatus
import Amazonka.Panorama.Types.TemplateType
import qualified Amazonka.Prelude as Prelude

-- | A job to create a camera stream node.
--
-- /See:/ 'newNodeFromTemplateJob' smart constructor.
data NodeFromTemplateJob = NodeFromTemplateJob'
  { -- | The job\'s template type.
    templateType :: Prelude.Maybe TemplateType,
    -- | When the job was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The job\'s ID.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The job\'s status.
    status :: Prelude.Maybe NodeFromTemplateJobStatus,
    -- | The node\'s name.
    nodeName :: Prelude.Maybe Prelude.Text,
    -- | The job\'s status message.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeFromTemplateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateType', 'nodeFromTemplateJob_templateType' - The job\'s template type.
--
-- 'createdTime', 'nodeFromTemplateJob_createdTime' - When the job was created.
--
-- 'jobId', 'nodeFromTemplateJob_jobId' - The job\'s ID.
--
-- 'status', 'nodeFromTemplateJob_status' - The job\'s status.
--
-- 'nodeName', 'nodeFromTemplateJob_nodeName' - The node\'s name.
--
-- 'statusMessage', 'nodeFromTemplateJob_statusMessage' - The job\'s status message.
newNodeFromTemplateJob ::
  NodeFromTemplateJob
newNodeFromTemplateJob =
  NodeFromTemplateJob'
    { templateType =
        Prelude.Nothing,
      createdTime = Prelude.Nothing,
      jobId = Prelude.Nothing,
      status = Prelude.Nothing,
      nodeName = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The job\'s template type.
nodeFromTemplateJob_templateType :: Lens.Lens' NodeFromTemplateJob (Prelude.Maybe TemplateType)
nodeFromTemplateJob_templateType = Lens.lens (\NodeFromTemplateJob' {templateType} -> templateType) (\s@NodeFromTemplateJob' {} a -> s {templateType = a} :: NodeFromTemplateJob)

-- | When the job was created.
nodeFromTemplateJob_createdTime :: Lens.Lens' NodeFromTemplateJob (Prelude.Maybe Prelude.UTCTime)
nodeFromTemplateJob_createdTime = Lens.lens (\NodeFromTemplateJob' {createdTime} -> createdTime) (\s@NodeFromTemplateJob' {} a -> s {createdTime = a} :: NodeFromTemplateJob) Prelude.. Lens.mapping Core._Time

-- | The job\'s ID.
nodeFromTemplateJob_jobId :: Lens.Lens' NodeFromTemplateJob (Prelude.Maybe Prelude.Text)
nodeFromTemplateJob_jobId = Lens.lens (\NodeFromTemplateJob' {jobId} -> jobId) (\s@NodeFromTemplateJob' {} a -> s {jobId = a} :: NodeFromTemplateJob)

-- | The job\'s status.
nodeFromTemplateJob_status :: Lens.Lens' NodeFromTemplateJob (Prelude.Maybe NodeFromTemplateJobStatus)
nodeFromTemplateJob_status = Lens.lens (\NodeFromTemplateJob' {status} -> status) (\s@NodeFromTemplateJob' {} a -> s {status = a} :: NodeFromTemplateJob)

-- | The node\'s name.
nodeFromTemplateJob_nodeName :: Lens.Lens' NodeFromTemplateJob (Prelude.Maybe Prelude.Text)
nodeFromTemplateJob_nodeName = Lens.lens (\NodeFromTemplateJob' {nodeName} -> nodeName) (\s@NodeFromTemplateJob' {} a -> s {nodeName = a} :: NodeFromTemplateJob)

-- | The job\'s status message.
nodeFromTemplateJob_statusMessage :: Lens.Lens' NodeFromTemplateJob (Prelude.Maybe Prelude.Text)
nodeFromTemplateJob_statusMessage = Lens.lens (\NodeFromTemplateJob' {statusMessage} -> statusMessage) (\s@NodeFromTemplateJob' {} a -> s {statusMessage = a} :: NodeFromTemplateJob)

instance Core.FromJSON NodeFromTemplateJob where
  parseJSON =
    Core.withObject
      "NodeFromTemplateJob"
      ( \x ->
          NodeFromTemplateJob'
            Prelude.<$> (x Core..:? "TemplateType")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "JobId")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "NodeName")
            Prelude.<*> (x Core..:? "StatusMessage")
      )

instance Prelude.Hashable NodeFromTemplateJob where
  hashWithSalt _salt NodeFromTemplateJob' {..} =
    _salt `Prelude.hashWithSalt` templateType
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` nodeName
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData NodeFromTemplateJob where
  rnf NodeFromTemplateJob' {..} =
    Prelude.rnf templateType
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf nodeName
      `Prelude.seq` Prelude.rnf statusMessage
