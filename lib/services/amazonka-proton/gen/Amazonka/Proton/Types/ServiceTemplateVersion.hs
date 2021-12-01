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
-- Module      : Amazonka.Proton.Types.ServiceTemplateVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ServiceTemplateVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.CompatibleEnvironmentTemplate
import Amazonka.Proton.Types.TemplateVersionStatus

-- | The version of a service template detail data.
--
-- /See:/ 'newServiceTemplateVersion' smart constructor.
data ServiceTemplateVersion = ServiceTemplateVersion'
  { -- | The schema of the version of a service template.
    schema :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A service template version status message.
    statusMessage :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of the recommended minor version of the service template.
    recommendedMinorVersion :: Prelude.Maybe Prelude.Text,
    -- | A description of the version of a service template.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the version of a service template.
    arn :: Prelude.Text,
    -- | An array of compatible environment template names for the major version
    -- of a service template.
    compatibleEnvironmentTemplates :: [CompatibleEnvironmentTemplate],
    -- | The time when the version of a service template was created.
    createdAt :: Core.POSIX,
    -- | The time when the version of a service template was last modified.
    lastModifiedAt :: Core.POSIX,
    -- | The ID of the latest major version that\'s associated with the version
    -- of a service template.
    majorVersion :: Prelude.Text,
    -- | The ID of the minor version of a service template.
    minorVersion :: Prelude.Text,
    -- | The service template version status.
    status :: TemplateVersionStatus,
    -- | The name of the version of a service template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceTemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schema', 'serviceTemplateVersion_schema' - The schema of the version of a service template.
--
-- 'statusMessage', 'serviceTemplateVersion_statusMessage' - A service template version status message.
--
-- 'recommendedMinorVersion', 'serviceTemplateVersion_recommendedMinorVersion' - The ID of the recommended minor version of the service template.
--
-- 'description', 'serviceTemplateVersion_description' - A description of the version of a service template.
--
-- 'arn', 'serviceTemplateVersion_arn' - The Amazon Resource Name (ARN) of the version of a service template.
--
-- 'compatibleEnvironmentTemplates', 'serviceTemplateVersion_compatibleEnvironmentTemplates' - An array of compatible environment template names for the major version
-- of a service template.
--
-- 'createdAt', 'serviceTemplateVersion_createdAt' - The time when the version of a service template was created.
--
-- 'lastModifiedAt', 'serviceTemplateVersion_lastModifiedAt' - The time when the version of a service template was last modified.
--
-- 'majorVersion', 'serviceTemplateVersion_majorVersion' - The ID of the latest major version that\'s associated with the version
-- of a service template.
--
-- 'minorVersion', 'serviceTemplateVersion_minorVersion' - The ID of the minor version of a service template.
--
-- 'status', 'serviceTemplateVersion_status' - The service template version status.
--
-- 'templateName', 'serviceTemplateVersion_templateName' - The name of the version of a service template.
newServiceTemplateVersion ::
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
  ServiceTemplateVersion
newServiceTemplateVersion
  pArn_
  pCreatedAt_
  pLastModifiedAt_
  pMajorVersion_
  pMinorVersion_
  pStatus_
  pTemplateName_ =
    ServiceTemplateVersion'
      { schema = Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        recommendedMinorVersion = Prelude.Nothing,
        description = Prelude.Nothing,
        arn = pArn_,
        compatibleEnvironmentTemplates = Prelude.mempty,
        createdAt = Core._Time Lens.# pCreatedAt_,
        lastModifiedAt = Core._Time Lens.# pLastModifiedAt_,
        majorVersion = pMajorVersion_,
        minorVersion = pMinorVersion_,
        status = pStatus_,
        templateName = pTemplateName_
      }

-- | The schema of the version of a service template.
serviceTemplateVersion_schema :: Lens.Lens' ServiceTemplateVersion (Prelude.Maybe Prelude.Text)
serviceTemplateVersion_schema = Lens.lens (\ServiceTemplateVersion' {schema} -> schema) (\s@ServiceTemplateVersion' {} a -> s {schema = a} :: ServiceTemplateVersion) Prelude.. Lens.mapping Core._Sensitive

-- | A service template version status message.
serviceTemplateVersion_statusMessage :: Lens.Lens' ServiceTemplateVersion (Prelude.Maybe Prelude.Text)
serviceTemplateVersion_statusMessage = Lens.lens (\ServiceTemplateVersion' {statusMessage} -> statusMessage) (\s@ServiceTemplateVersion' {} a -> s {statusMessage = a} :: ServiceTemplateVersion) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of the recommended minor version of the service template.
serviceTemplateVersion_recommendedMinorVersion :: Lens.Lens' ServiceTemplateVersion (Prelude.Maybe Prelude.Text)
serviceTemplateVersion_recommendedMinorVersion = Lens.lens (\ServiceTemplateVersion' {recommendedMinorVersion} -> recommendedMinorVersion) (\s@ServiceTemplateVersion' {} a -> s {recommendedMinorVersion = a} :: ServiceTemplateVersion)

-- | A description of the version of a service template.
serviceTemplateVersion_description :: Lens.Lens' ServiceTemplateVersion (Prelude.Maybe Prelude.Text)
serviceTemplateVersion_description = Lens.lens (\ServiceTemplateVersion' {description} -> description) (\s@ServiceTemplateVersion' {} a -> s {description = a} :: ServiceTemplateVersion) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) of the version of a service template.
serviceTemplateVersion_arn :: Lens.Lens' ServiceTemplateVersion Prelude.Text
serviceTemplateVersion_arn = Lens.lens (\ServiceTemplateVersion' {arn} -> arn) (\s@ServiceTemplateVersion' {} a -> s {arn = a} :: ServiceTemplateVersion)

-- | An array of compatible environment template names for the major version
-- of a service template.
serviceTemplateVersion_compatibleEnvironmentTemplates :: Lens.Lens' ServiceTemplateVersion [CompatibleEnvironmentTemplate]
serviceTemplateVersion_compatibleEnvironmentTemplates = Lens.lens (\ServiceTemplateVersion' {compatibleEnvironmentTemplates} -> compatibleEnvironmentTemplates) (\s@ServiceTemplateVersion' {} a -> s {compatibleEnvironmentTemplates = a} :: ServiceTemplateVersion) Prelude.. Lens.coerced

-- | The time when the version of a service template was created.
serviceTemplateVersion_createdAt :: Lens.Lens' ServiceTemplateVersion Prelude.UTCTime
serviceTemplateVersion_createdAt = Lens.lens (\ServiceTemplateVersion' {createdAt} -> createdAt) (\s@ServiceTemplateVersion' {} a -> s {createdAt = a} :: ServiceTemplateVersion) Prelude.. Core._Time

-- | The time when the version of a service template was last modified.
serviceTemplateVersion_lastModifiedAt :: Lens.Lens' ServiceTemplateVersion Prelude.UTCTime
serviceTemplateVersion_lastModifiedAt = Lens.lens (\ServiceTemplateVersion' {lastModifiedAt} -> lastModifiedAt) (\s@ServiceTemplateVersion' {} a -> s {lastModifiedAt = a} :: ServiceTemplateVersion) Prelude.. Core._Time

-- | The ID of the latest major version that\'s associated with the version
-- of a service template.
serviceTemplateVersion_majorVersion :: Lens.Lens' ServiceTemplateVersion Prelude.Text
serviceTemplateVersion_majorVersion = Lens.lens (\ServiceTemplateVersion' {majorVersion} -> majorVersion) (\s@ServiceTemplateVersion' {} a -> s {majorVersion = a} :: ServiceTemplateVersion)

-- | The ID of the minor version of a service template.
serviceTemplateVersion_minorVersion :: Lens.Lens' ServiceTemplateVersion Prelude.Text
serviceTemplateVersion_minorVersion = Lens.lens (\ServiceTemplateVersion' {minorVersion} -> minorVersion) (\s@ServiceTemplateVersion' {} a -> s {minorVersion = a} :: ServiceTemplateVersion)

-- | The service template version status.
serviceTemplateVersion_status :: Lens.Lens' ServiceTemplateVersion TemplateVersionStatus
serviceTemplateVersion_status = Lens.lens (\ServiceTemplateVersion' {status} -> status) (\s@ServiceTemplateVersion' {} a -> s {status = a} :: ServiceTemplateVersion)

-- | The name of the version of a service template.
serviceTemplateVersion_templateName :: Lens.Lens' ServiceTemplateVersion Prelude.Text
serviceTemplateVersion_templateName = Lens.lens (\ServiceTemplateVersion' {templateName} -> templateName) (\s@ServiceTemplateVersion' {} a -> s {templateName = a} :: ServiceTemplateVersion)

instance Core.FromJSON ServiceTemplateVersion where
  parseJSON =
    Core.withObject
      "ServiceTemplateVersion"
      ( \x ->
          ServiceTemplateVersion'
            Prelude.<$> (x Core..:? "schema")
            Prelude.<*> (x Core..:? "statusMessage")
            Prelude.<*> (x Core..:? "recommendedMinorVersion")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> ( x Core..:? "compatibleEnvironmentTemplates"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "lastModifiedAt")
            Prelude.<*> (x Core..: "majorVersion")
            Prelude.<*> (x Core..: "minorVersion")
            Prelude.<*> (x Core..: "status")
            Prelude.<*> (x Core..: "templateName")
      )

instance Prelude.Hashable ServiceTemplateVersion where
  hashWithSalt salt' ServiceTemplateVersion' {..} =
    salt' `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` minorVersion
      `Prelude.hashWithSalt` majorVersion
      `Prelude.hashWithSalt` lastModifiedAt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` compatibleEnvironmentTemplates
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` recommendedMinorVersion
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` schema

instance Prelude.NFData ServiceTemplateVersion where
  rnf ServiceTemplateVersion' {..} =
    Prelude.rnf schema
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf minorVersion
      `Prelude.seq` Prelude.rnf majorVersion
      `Prelude.seq` Prelude.rnf lastModifiedAt
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf compatibleEnvironmentTemplates
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf recommendedMinorVersion
      `Prelude.seq` Prelude.rnf statusMessage
