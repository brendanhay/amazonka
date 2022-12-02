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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.EnvironmentTemplateVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.TemplateVersionStatus

-- | The environment template version data.
--
-- /See:/ 'newEnvironmentTemplateVersion' smart constructor.
data EnvironmentTemplateVersion = EnvironmentTemplateVersion'
  { -- | A description of the minor version of an environment template.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The recommended minor version of the environment template.
    recommendedMinorVersion :: Prelude.Maybe Prelude.Text,
    -- | The schema of the version of an environment template.
    schema :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The status message of the version of an environment template.
    statusMessage :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the version of an environment
    -- template.
    arn :: Prelude.Text,
    -- | The time when the version of an environment template was created.
    createdAt :: Data.POSIX,
    -- | The time when the version of an environment template was last modified.
    lastModifiedAt :: Data.POSIX,
    -- | The latest major version that\'s associated with the version of an
    -- environment template.
    majorVersion :: Prelude.Text,
    -- | The minor version of an environment template.
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
-- 'description', 'environmentTemplateVersion_description' - A description of the minor version of an environment template.
--
-- 'recommendedMinorVersion', 'environmentTemplateVersion_recommendedMinorVersion' - The recommended minor version of the environment template.
--
-- 'schema', 'environmentTemplateVersion_schema' - The schema of the version of an environment template.
--
-- 'statusMessage', 'environmentTemplateVersion_statusMessage' - The status message of the version of an environment template.
--
-- 'arn', 'environmentTemplateVersion_arn' - The Amazon Resource Name (ARN) of the version of an environment
-- template.
--
-- 'createdAt', 'environmentTemplateVersion_createdAt' - The time when the version of an environment template was created.
--
-- 'lastModifiedAt', 'environmentTemplateVersion_lastModifiedAt' - The time when the version of an environment template was last modified.
--
-- 'majorVersion', 'environmentTemplateVersion_majorVersion' - The latest major version that\'s associated with the version of an
-- environment template.
--
-- 'minorVersion', 'environmentTemplateVersion_minorVersion' - The minor version of an environment template.
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
      { description =
          Prelude.Nothing,
        recommendedMinorVersion = Prelude.Nothing,
        schema = Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        lastModifiedAt =
          Data._Time Lens.# pLastModifiedAt_,
        majorVersion = pMajorVersion_,
        minorVersion = pMinorVersion_,
        status = pStatus_,
        templateName = pTemplateName_
      }

-- | A description of the minor version of an environment template.
environmentTemplateVersion_description :: Lens.Lens' EnvironmentTemplateVersion (Prelude.Maybe Prelude.Text)
environmentTemplateVersion_description = Lens.lens (\EnvironmentTemplateVersion' {description} -> description) (\s@EnvironmentTemplateVersion' {} a -> s {description = a} :: EnvironmentTemplateVersion) Prelude.. Lens.mapping Data._Sensitive

-- | The recommended minor version of the environment template.
environmentTemplateVersion_recommendedMinorVersion :: Lens.Lens' EnvironmentTemplateVersion (Prelude.Maybe Prelude.Text)
environmentTemplateVersion_recommendedMinorVersion = Lens.lens (\EnvironmentTemplateVersion' {recommendedMinorVersion} -> recommendedMinorVersion) (\s@EnvironmentTemplateVersion' {} a -> s {recommendedMinorVersion = a} :: EnvironmentTemplateVersion)

-- | The schema of the version of an environment template.
environmentTemplateVersion_schema :: Lens.Lens' EnvironmentTemplateVersion (Prelude.Maybe Prelude.Text)
environmentTemplateVersion_schema = Lens.lens (\EnvironmentTemplateVersion' {schema} -> schema) (\s@EnvironmentTemplateVersion' {} a -> s {schema = a} :: EnvironmentTemplateVersion) Prelude.. Lens.mapping Data._Sensitive

-- | The status message of the version of an environment template.
environmentTemplateVersion_statusMessage :: Lens.Lens' EnvironmentTemplateVersion (Prelude.Maybe Prelude.Text)
environmentTemplateVersion_statusMessage = Lens.lens (\EnvironmentTemplateVersion' {statusMessage} -> statusMessage) (\s@EnvironmentTemplateVersion' {} a -> s {statusMessage = a} :: EnvironmentTemplateVersion) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) of the version of an environment
-- template.
environmentTemplateVersion_arn :: Lens.Lens' EnvironmentTemplateVersion Prelude.Text
environmentTemplateVersion_arn = Lens.lens (\EnvironmentTemplateVersion' {arn} -> arn) (\s@EnvironmentTemplateVersion' {} a -> s {arn = a} :: EnvironmentTemplateVersion)

-- | The time when the version of an environment template was created.
environmentTemplateVersion_createdAt :: Lens.Lens' EnvironmentTemplateVersion Prelude.UTCTime
environmentTemplateVersion_createdAt = Lens.lens (\EnvironmentTemplateVersion' {createdAt} -> createdAt) (\s@EnvironmentTemplateVersion' {} a -> s {createdAt = a} :: EnvironmentTemplateVersion) Prelude.. Data._Time

-- | The time when the version of an environment template was last modified.
environmentTemplateVersion_lastModifiedAt :: Lens.Lens' EnvironmentTemplateVersion Prelude.UTCTime
environmentTemplateVersion_lastModifiedAt = Lens.lens (\EnvironmentTemplateVersion' {lastModifiedAt} -> lastModifiedAt) (\s@EnvironmentTemplateVersion' {} a -> s {lastModifiedAt = a} :: EnvironmentTemplateVersion) Prelude.. Data._Time

-- | The latest major version that\'s associated with the version of an
-- environment template.
environmentTemplateVersion_majorVersion :: Lens.Lens' EnvironmentTemplateVersion Prelude.Text
environmentTemplateVersion_majorVersion = Lens.lens (\EnvironmentTemplateVersion' {majorVersion} -> majorVersion) (\s@EnvironmentTemplateVersion' {} a -> s {majorVersion = a} :: EnvironmentTemplateVersion)

-- | The minor version of an environment template.
environmentTemplateVersion_minorVersion :: Lens.Lens' EnvironmentTemplateVersion Prelude.Text
environmentTemplateVersion_minorVersion = Lens.lens (\EnvironmentTemplateVersion' {minorVersion} -> minorVersion) (\s@EnvironmentTemplateVersion' {} a -> s {minorVersion = a} :: EnvironmentTemplateVersion)

-- | The status of the version of an environment template.
environmentTemplateVersion_status :: Lens.Lens' EnvironmentTemplateVersion TemplateVersionStatus
environmentTemplateVersion_status = Lens.lens (\EnvironmentTemplateVersion' {status} -> status) (\s@EnvironmentTemplateVersion' {} a -> s {status = a} :: EnvironmentTemplateVersion)

-- | The name of the version of an environment template.
environmentTemplateVersion_templateName :: Lens.Lens' EnvironmentTemplateVersion Prelude.Text
environmentTemplateVersion_templateName = Lens.lens (\EnvironmentTemplateVersion' {templateName} -> templateName) (\s@EnvironmentTemplateVersion' {} a -> s {templateName = a} :: EnvironmentTemplateVersion)

instance Data.FromJSON EnvironmentTemplateVersion where
  parseJSON =
    Data.withObject
      "EnvironmentTemplateVersion"
      ( \x ->
          EnvironmentTemplateVersion'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "recommendedMinorVersion")
            Prelude.<*> (x Data..:? "schema")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastModifiedAt")
            Prelude.<*> (x Data..: "majorVersion")
            Prelude.<*> (x Data..: "minorVersion")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "templateName")
      )

instance Prelude.Hashable EnvironmentTemplateVersion where
  hashWithSalt _salt EnvironmentTemplateVersion' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` recommendedMinorVersion
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastModifiedAt
      `Prelude.hashWithSalt` majorVersion
      `Prelude.hashWithSalt` minorVersion
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData EnvironmentTemplateVersion where
  rnf EnvironmentTemplateVersion' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf recommendedMinorVersion
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModifiedAt
      `Prelude.seq` Prelude.rnf majorVersion
      `Prelude.seq` Prelude.rnf minorVersion
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf templateName
