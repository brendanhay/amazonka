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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.JobTemplateSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about the job template.
--
-- /See:/ 'newJobTemplateSummary' smart constructor.
data JobTemplateSummary = JobTemplateSummary'
  { -- | The ARN of the job template.
    jobTemplateArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the job template.
    jobTemplateId :: Prelude.Maybe Prelude.Text,
    -- | A description of the job template.
    description :: Prelude.Maybe Prelude.Text,
    -- | The time, in seconds since the epoch, when the job template was created.
    createdAt :: Prelude.Maybe Core.POSIX
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
-- 'jobTemplateArn', 'jobTemplateSummary_jobTemplateArn' - The ARN of the job template.
--
-- 'jobTemplateId', 'jobTemplateSummary_jobTemplateId' - The unique identifier of the job template.
--
-- 'description', 'jobTemplateSummary_description' - A description of the job template.
--
-- 'createdAt', 'jobTemplateSummary_createdAt' - The time, in seconds since the epoch, when the job template was created.
newJobTemplateSummary ::
  JobTemplateSummary
newJobTemplateSummary =
  JobTemplateSummary'
    { jobTemplateArn =
        Prelude.Nothing,
      jobTemplateId = Prelude.Nothing,
      description = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The ARN of the job template.
jobTemplateSummary_jobTemplateArn :: Lens.Lens' JobTemplateSummary (Prelude.Maybe Prelude.Text)
jobTemplateSummary_jobTemplateArn = Lens.lens (\JobTemplateSummary' {jobTemplateArn} -> jobTemplateArn) (\s@JobTemplateSummary' {} a -> s {jobTemplateArn = a} :: JobTemplateSummary)

-- | The unique identifier of the job template.
jobTemplateSummary_jobTemplateId :: Lens.Lens' JobTemplateSummary (Prelude.Maybe Prelude.Text)
jobTemplateSummary_jobTemplateId = Lens.lens (\JobTemplateSummary' {jobTemplateId} -> jobTemplateId) (\s@JobTemplateSummary' {} a -> s {jobTemplateId = a} :: JobTemplateSummary)

-- | A description of the job template.
jobTemplateSummary_description :: Lens.Lens' JobTemplateSummary (Prelude.Maybe Prelude.Text)
jobTemplateSummary_description = Lens.lens (\JobTemplateSummary' {description} -> description) (\s@JobTemplateSummary' {} a -> s {description = a} :: JobTemplateSummary)

-- | The time, in seconds since the epoch, when the job template was created.
jobTemplateSummary_createdAt :: Lens.Lens' JobTemplateSummary (Prelude.Maybe Prelude.UTCTime)
jobTemplateSummary_createdAt = Lens.lens (\JobTemplateSummary' {createdAt} -> createdAt) (\s@JobTemplateSummary' {} a -> s {createdAt = a} :: JobTemplateSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON JobTemplateSummary where
  parseJSON =
    Core.withObject
      "JobTemplateSummary"
      ( \x ->
          JobTemplateSummary'
            Prelude.<$> (x Core..:? "jobTemplateArn")
            Prelude.<*> (x Core..:? "jobTemplateId")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "createdAt")
      )

instance Prelude.Hashable JobTemplateSummary where
  hashWithSalt _salt JobTemplateSummary' {..} =
    _salt `Prelude.hashWithSalt` jobTemplateArn
      `Prelude.hashWithSalt` jobTemplateId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData JobTemplateSummary where
  rnf JobTemplateSummary' {..} =
    Prelude.rnf jobTemplateArn
      `Prelude.seq` Prelude.rnf jobTemplateId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf createdAt
