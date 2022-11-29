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
-- Module      : Amazonka.EMRContainers.Types.JobTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.JobTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMRContainers.Types.JobTemplateData
import qualified Amazonka.Prelude as Prelude

-- | This entity describes a job template. Job template stores values of
-- StartJobRun API request in a template and can be used to start a job
-- run. Job template allows two use cases: avoid repeating recurring
-- StartJobRun API request values, enforcing certain values in StartJobRun
-- API request.
--
-- /See:/ 'newJobTemplate' smart constructor.
data JobTemplate = JobTemplate'
  { -- | The tags assigned to the job template.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the job template.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the job template.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the job template.
    id :: Prelude.Maybe Prelude.Text,
    -- | The KMS key ARN used to encrypt the job template.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The user who created the job template.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the job template was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The error message in case the decryption of job template fails.
    decryptionError :: Prelude.Maybe Prelude.Text,
    -- | The job template data which holds values of StartJobRun API request.
    jobTemplateData :: JobTemplateData
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'jobTemplate_tags' - The tags assigned to the job template.
--
-- 'name', 'jobTemplate_name' - The name of the job template.
--
-- 'arn', 'jobTemplate_arn' - The ARN of the job template.
--
-- 'id', 'jobTemplate_id' - The ID of the job template.
--
-- 'kmsKeyArn', 'jobTemplate_kmsKeyArn' - The KMS key ARN used to encrypt the job template.
--
-- 'createdBy', 'jobTemplate_createdBy' - The user who created the job template.
--
-- 'createdAt', 'jobTemplate_createdAt' - The date and time when the job template was created.
--
-- 'decryptionError', 'jobTemplate_decryptionError' - The error message in case the decryption of job template fails.
--
-- 'jobTemplateData', 'jobTemplate_jobTemplateData' - The job template data which holds values of StartJobRun API request.
newJobTemplate ::
  -- | 'jobTemplateData'
  JobTemplateData ->
  JobTemplate
newJobTemplate pJobTemplateData_ =
  JobTemplate'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      decryptionError = Prelude.Nothing,
      jobTemplateData = pJobTemplateData_
    }

-- | The tags assigned to the job template.
jobTemplate_tags :: Lens.Lens' JobTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobTemplate_tags = Lens.lens (\JobTemplate' {tags} -> tags) (\s@JobTemplate' {} a -> s {tags = a} :: JobTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The name of the job template.
jobTemplate_name :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.Text)
jobTemplate_name = Lens.lens (\JobTemplate' {name} -> name) (\s@JobTemplate' {} a -> s {name = a} :: JobTemplate)

-- | The ARN of the job template.
jobTemplate_arn :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.Text)
jobTemplate_arn = Lens.lens (\JobTemplate' {arn} -> arn) (\s@JobTemplate' {} a -> s {arn = a} :: JobTemplate)

-- | The ID of the job template.
jobTemplate_id :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.Text)
jobTemplate_id = Lens.lens (\JobTemplate' {id} -> id) (\s@JobTemplate' {} a -> s {id = a} :: JobTemplate)

-- | The KMS key ARN used to encrypt the job template.
jobTemplate_kmsKeyArn :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.Text)
jobTemplate_kmsKeyArn = Lens.lens (\JobTemplate' {kmsKeyArn} -> kmsKeyArn) (\s@JobTemplate' {} a -> s {kmsKeyArn = a} :: JobTemplate)

-- | The user who created the job template.
jobTemplate_createdBy :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.Text)
jobTemplate_createdBy = Lens.lens (\JobTemplate' {createdBy} -> createdBy) (\s@JobTemplate' {} a -> s {createdBy = a} :: JobTemplate)

-- | The date and time when the job template was created.
jobTemplate_createdAt :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.UTCTime)
jobTemplate_createdAt = Lens.lens (\JobTemplate' {createdAt} -> createdAt) (\s@JobTemplate' {} a -> s {createdAt = a} :: JobTemplate) Prelude.. Lens.mapping Core._Time

-- | The error message in case the decryption of job template fails.
jobTemplate_decryptionError :: Lens.Lens' JobTemplate (Prelude.Maybe Prelude.Text)
jobTemplate_decryptionError = Lens.lens (\JobTemplate' {decryptionError} -> decryptionError) (\s@JobTemplate' {} a -> s {decryptionError = a} :: JobTemplate)

-- | The job template data which holds values of StartJobRun API request.
jobTemplate_jobTemplateData :: Lens.Lens' JobTemplate JobTemplateData
jobTemplate_jobTemplateData = Lens.lens (\JobTemplate' {jobTemplateData} -> jobTemplateData) (\s@JobTemplate' {} a -> s {jobTemplateData = a} :: JobTemplate)

instance Core.FromJSON JobTemplate where
  parseJSON =
    Core.withObject
      "JobTemplate"
      ( \x ->
          JobTemplate'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "kmsKeyArn")
            Prelude.<*> (x Core..:? "createdBy")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "decryptionError")
            Prelude.<*> (x Core..: "jobTemplateData")
      )

instance Prelude.Hashable JobTemplate where
  hashWithSalt _salt JobTemplate' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` decryptionError
      `Prelude.hashWithSalt` jobTemplateData

instance Prelude.NFData JobTemplate where
  rnf JobTemplate' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf decryptionError
      `Prelude.seq` Prelude.rnf jobTemplateData
