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
-- Module      : Amazonka.IoT.Types.JobTemplateSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.JobTemplateSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about the job template.
--
-- /See:/ 'newJobTemplateSummary' smart constructor.
data JobTemplateSummary = JobTemplateSummary'
  { -- | The time, in seconds since the epoch, when the job template was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier of the job template.
    jobTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the job template.
    jobTemplateArn :: Prelude.Maybe Prelude.Text,
    -- | A description of the job template.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobTemplateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'jobTemplateSummary_createdAt' - The time, in seconds since the epoch, when the job template was created.
--
-- 'jobTemplateId', 'jobTemplateSummary_jobTemplateId' - The unique identifier of the job template.
--
-- 'jobTemplateArn', 'jobTemplateSummary_jobTemplateArn' - The ARN of the job template.
--
-- 'description', 'jobTemplateSummary_description' - A description of the job template.
newJobTemplateSummary ::
  JobTemplateSummary
newJobTemplateSummary =
  JobTemplateSummary'
    { createdAt = Prelude.Nothing,
      jobTemplateId = Prelude.Nothing,
      jobTemplateArn = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The time, in seconds since the epoch, when the job template was created.
jobTemplateSummary_createdAt :: Lens.Lens' JobTemplateSummary (Prelude.Maybe Prelude.UTCTime)
jobTemplateSummary_createdAt = Lens.lens (\JobTemplateSummary' {createdAt} -> createdAt) (\s@JobTemplateSummary' {} a -> s {createdAt = a} :: JobTemplateSummary) Prelude.. Lens.mapping Core._Time

-- | The unique identifier of the job template.
jobTemplateSummary_jobTemplateId :: Lens.Lens' JobTemplateSummary (Prelude.Maybe Prelude.Text)
jobTemplateSummary_jobTemplateId = Lens.lens (\JobTemplateSummary' {jobTemplateId} -> jobTemplateId) (\s@JobTemplateSummary' {} a -> s {jobTemplateId = a} :: JobTemplateSummary)

-- | The ARN of the job template.
jobTemplateSummary_jobTemplateArn :: Lens.Lens' JobTemplateSummary (Prelude.Maybe Prelude.Text)
jobTemplateSummary_jobTemplateArn = Lens.lens (\JobTemplateSummary' {jobTemplateArn} -> jobTemplateArn) (\s@JobTemplateSummary' {} a -> s {jobTemplateArn = a} :: JobTemplateSummary)

-- | A description of the job template.
jobTemplateSummary_description :: Lens.Lens' JobTemplateSummary (Prelude.Maybe Prelude.Text)
jobTemplateSummary_description = Lens.lens (\JobTemplateSummary' {description} -> description) (\s@JobTemplateSummary' {} a -> s {description = a} :: JobTemplateSummary)

instance Core.FromJSON JobTemplateSummary where
  parseJSON =
    Core.withObject
      "JobTemplateSummary"
      ( \x ->
          JobTemplateSummary'
            Prelude.<$> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "jobTemplateId")
            Prelude.<*> (x Core..:? "jobTemplateArn")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable JobTemplateSummary

instance Prelude.NFData JobTemplateSummary
