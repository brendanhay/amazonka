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
-- Module      : Amazonka.Proton.Types.EnvironmentTemplateVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.EnvironmentTemplateVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.TemplateVersionStatus

-- | The environment template version data.
--
-- /See:/ 'newEnvironmentTemplateVersion' smart constructor.
data EnvironmentTemplateVersion = EnvironmentTemplateVersion'
  { -- | The schema of the version of an environment template.
    schema :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The status message of the version of an environment template.
    statusMessage :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of the recommended minor version of the environment template.
    recommendedMinorVersion :: Prelude.Maybe Prelude.Text,
    -- | A description of the minor version of an environment template.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the version of an environment
    -- template.
    arn :: Prelude.Text,
    -- | The time when the version of an environment template was created.
    createdAt :: Core.POSIX,
    -- | The time when the version of an environment template was last modified.
    lastModifiedAt :: Core.POSIX,
    -- | The ID of the latest major version that\'s associated with the version
    -- of an environment template.
    majorVersion :: Prelude.Text,
    -- | The ID of the minor version of an environment template.
    minorVersion :: Prelude.Text,
    -- | The status of the version of an environment template.
    status :: TemplateVersionStatus,
    -- | The name of the version of an environment template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentTemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schema', 'environmentTemplateVersion_schema' - The schema of the version of an environment template.
--
-- 'statusMessage', 'environmentTemplateVersion_statusMessage' - The status message of the version of an environment template.
--
-- 'recommendedMinorVersion', 'environmentTemplateVersion_recommendedMinorVersion' - The ID of the recommended minor version of the environment template.
--
-- 'description', 'environmentTemplateVersion_description' - A description of the minor version of an environment template.
--
-- 'arn', 'environmentTemplateVersion_arn' - The Amazon Resource Name (ARN) of the version of an environment
-- template.
--
-- 'createdAt', 'environmentTemplateVersion_createdAt' - The time when the version of an environment template was created.
--
-- 'lastModifiedAt', 'environmentTemplateVersion_lastModifiedAt' - The time when the version of an environment template was last modified.
--
-- 'majorVersion', 'environmentTemplateVersion_majorVersion' - The ID of the latest major version that\'s associated with the version
-- of an environment template.
--
-- 'minorVersion', 'environmentTemplateVersion_minorVersion' - The ID of the minor version of an environment template.
--
-- 'status', 'environmentTemplateVersion_status' - The status of the version of an environment template.
--
-- 'templateName', 'environmentTemplateVersion_templateName' - The name of the version of an environment template.
newEnvironmentTemplateVersion ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModifiedAt'
  Prelude.UTCTime ->
  -- | 'majorVersion'
  Prelude.Text ->
  -- | 'minorVersion'
  Prelude.Text ->
  -- | 'status'
  TemplateVersionStatus ->
  -- | 'templateName'
  Prelude.Text ->
  EnvironmentTemplateVersion
newEnvironmentTemplateVersion
  pArn_
  pCreatedAt_
  pLastModifiedAt_
  pMajorVersion_
  pMinorVersion_
  pStatus_
  pTemplateName_ =
    EnvironmentTemplateVersion'
      { schema =
          Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        recommendedMinorVersion = Prelude.Nothing,
        description = Prelude.Nothing,
        arn = pArn_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        lastModifiedAt =
          Core._Time Lens.# pLastModifiedAt_,
        majorVersion = pMajorVersion_,
        minorVersion = pMinorVersion_,
        status = pStatus_,
        templateName = pTemplateName_
      }

-- | The schema of the version of an environment template.
environmentTemplateVersion_schema :: Lens.Lens' EnvironmentTemplateVersion (Prelude.Maybe Prelude.Text)
environmentTemplateVersion_schema = Lens.lens (\EnvironmentTemplateVersion' {schema} -> schema) (\s@EnvironmentTemplateVersion' {} a -> s {schema = a} :: EnvironmentTemplateVersion) Prelude.. Lens.mapping Core._Sensitive

-- | The status message of the version of an environment template.
environmentTemplateVersion_statusMessage :: Lens.Lens' EnvironmentTemplateVersion (Prelude.Maybe Prelude.Text)
environmentTemplateVersion_statusMessage = Lens.lens (\EnvironmentTemplateVersion' {statusMessage} -> statusMessage) (\s@EnvironmentTemplateVersion' {} a -> s {statusMessage = a} :: EnvironmentTemplateVersion) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of the recommended minor version of the environment template.
environmentTemplateVersion_recommendedMinorVersion :: Lens.Lens' EnvironmentTemplateVersion (Prelude.Maybe Prelude.Text)
environmentTemplateVersion_recommendedMinorVersion = Lens.lens (\EnvironmentTemplateVersion' {recommendedMinorVersion} -> recommendedMinorVersion) (\s@EnvironmentTemplateVersion' {} a -> s {recommendedMinorVersion = a} :: EnvironmentTemplateVersion)

-- | A description of the minor version of an environment template.
environmentTemplateVersion_description :: Lens.Lens' EnvironmentTemplateVersion (Prelude.Maybe Prelude.Text)
environmentTemplateVersion_description = Lens.lens (\EnvironmentTemplateVersion' {description} -> description) (\s@EnvironmentTemplateVersion' {} a -> s {description = a} :: EnvironmentTemplateVersion) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) of the version of an environment
-- template.
environmentTemplateVersion_arn :: Lens.Lens' EnvironmentTemplateVersion Prelude.Text
environmentTemplateVersion_arn = Lens.lens (\EnvironmentTemplateVersion' {arn} -> arn) (\s@EnvironmentTemplateVersion' {} a -> s {arn = a} :: EnvironmentTemplateVersion)

-- | The time when the version of an environment template was created.
environmentTemplateVersion_createdAt :: Lens.Lens' EnvironmentTemplateVersion Prelude.UTCTime
environmentTemplateVersion_createdAt = Lens.lens (\EnvironmentTemplateVersion' {createdAt} -> createdAt) (\s@EnvironmentTemplateVersion' {} a -> s {createdAt = a} :: EnvironmentTemplateVersion) Prelude.. Core._Time

-- | The time when the version of an environment template was last modified.
environmentTemplateVersion_lastModifiedAt :: Lens.Lens' EnvironmentTemplateVersion Prelude.UTCTime
environmentTemplateVersion_lastModifiedAt = Lens.lens (\EnvironmentTemplateVersion' {lastModifiedAt} -> lastModifiedAt) (\s@EnvironmentTemplateVersion' {} a -> s {lastModifiedAt = a} :: EnvironmentTemplateVersion) Prelude.. Core._Time

-- | The ID of the latest major version that\'s associated with the version
-- of an environment template.
environmentTemplateVersion_majorVersion :: Lens.Lens' EnvironmentTemplateVersion Prelude.Text
environmentTemplateVersion_majorVersion = Lens.lens (\EnvironmentTemplateVersion' {majorVersion} -> majorVersion) (\s@EnvironmentTemplateVersion' {} a -> s {majorVersion = a} :: EnvironmentTemplateVersion)

-- | The ID of the minor version of an environment template.
environmentTemplateVersion_minorVersion :: Lens.Lens' EnvironmentTemplateVersion Prelude.Text
environmentTemplateVersion_minorVersion = Lens.lens (\EnvironmentTemplateVersion' {minorVersion} -> minorVersion) (\s@EnvironmentTemplateVersion' {} a -> s {minorVersion = a} :: EnvironmentTemplateVersion)

-- | The status of the version of an environment template.
environmentTemplateVersion_status :: Lens.Lens' EnvironmentTemplateVersion TemplateVersionStatus
environmentTemplateVersion_status = Lens.lens (\EnvironmentTemplateVersion' {status} -> status) (\s@EnvironmentTemplateVersion' {} a -> s {status = a} :: EnvironmentTemplateVersion)

-- | The name of the version of an environment template.
environmentTemplateVersion_templateName :: Lens.Lens' EnvironmentTemplateVersion Prelude.Text
environmentTemplateVersion_templateName = Lens.lens (\EnvironmentTemplateVersion' {templateName} -> templateName) (\s@EnvironmentTemplateVersion' {} a -> s {templateName = a} :: EnvironmentTemplateVersion)

instance Core.FromJSON EnvironmentTemplateVersion where
  parseJSON =
    Core.withObject
      "EnvironmentTemplateVersion"
      ( \x ->
          EnvironmentTemplateVersion'
            Prelude.<$> (x Core..:? "schema")
            Prelude.<*> (x Core..:? "statusMessage")
            Prelude.<*> (x Core..:? "recommendedMinorVersion")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "lastModifiedAt")
            Prelude.<*> (x Core..: "majorVersion")
            Prelude.<*> (x Core..: "minorVersion")
            Prelude.<*> (x Core..: "status")
            Prelude.<*> (x Core..: "templateName")
      )

instance Prelude.Hashable EnvironmentTemplateVersion where
  hashWithSalt salt' EnvironmentTemplateVersion' {..} =
    salt' `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` minorVersion
      `Prelude.hashWithSalt` majorVersion
      `Prelude.hashWithSalt` lastModifiedAt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` recommendedMinorVersion
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` schema

instance Prelude.NFData EnvironmentTemplateVersion where
  rnf EnvironmentTemplateVersion' {..} =
    Prelude.rnf schema
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf minorVersion
      `Prelude.seq` Prelude.rnf majorVersion
      `Prelude.seq` Prelude.rnf lastModifiedAt
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf recommendedMinorVersion
      `Prelude.seq` Prelude.rnf statusMessage
