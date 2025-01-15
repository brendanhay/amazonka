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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ServiceTemplateVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.CompatibleEnvironmentTemplate
import Amazonka.Proton.Types.ServiceTemplateSupportedComponentSourceType
import Amazonka.Proton.Types.TemplateVersionStatus

-- | Detailed data of an Proton service template version resource.
--
-- /See:/ 'newServiceTemplateVersion' smart constructor.
data ServiceTemplateVersion = ServiceTemplateVersion'
  { -- | A description of the version of a service template.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The recommended minor version of the service template.
    recommendedMinorVersion :: Prelude.Maybe Prelude.Text,
    -- | The schema of the version of a service template.
    schema :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A service template version status message.
    statusMessage :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | An array of supported component sources. Components with supported
    -- sources can be attached to service instances based on this service
    -- template version.
    --
    -- For more information about components, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
    -- in the /Proton User Guide/.
    supportedComponentSources :: Prelude.Maybe [ServiceTemplateSupportedComponentSourceType],
    -- | The Amazon Resource Name (ARN) of the version of a service template.
    arn :: Prelude.Text,
    -- | An array of compatible environment template names for the major version
    -- of a service template.
    compatibleEnvironmentTemplates :: [CompatibleEnvironmentTemplate],
    -- | The time when the version of a service template was created.
    createdAt :: Data.POSIX,
    -- | The time when the version of a service template was last modified.
    lastModifiedAt :: Data.POSIX,
    -- | The latest major version that\'s associated with the version of a
    -- service template.
    majorVersion :: Prelude.Text,
    -- | The minor version of a service template.
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
-- 'description', 'serviceTemplateVersion_description' - A description of the version of a service template.
--
-- 'recommendedMinorVersion', 'serviceTemplateVersion_recommendedMinorVersion' - The recommended minor version of the service template.
--
-- 'schema', 'serviceTemplateVersion_schema' - The schema of the version of a service template.
--
-- 'statusMessage', 'serviceTemplateVersion_statusMessage' - A service template version status message.
--
-- 'supportedComponentSources', 'serviceTemplateVersion_supportedComponentSources' - An array of supported component sources. Components with supported
-- sources can be attached to service instances based on this service
-- template version.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
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
-- 'majorVersion', 'serviceTemplateVersion_majorVersion' - The latest major version that\'s associated with the version of a
-- service template.
--
-- 'minorVersion', 'serviceTemplateVersion_minorVersion' - The minor version of a service template.
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
      { description =
          Prelude.Nothing,
        recommendedMinorVersion = Prelude.Nothing,
        schema = Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        supportedComponentSources = Prelude.Nothing,
        arn = pArn_,
        compatibleEnvironmentTemplates = Prelude.mempty,
        createdAt = Data._Time Lens.# pCreatedAt_,
        lastModifiedAt = Data._Time Lens.# pLastModifiedAt_,
        majorVersion = pMajorVersion_,
        minorVersion = pMinorVersion_,
        status = pStatus_,
        templateName = pTemplateName_
      }

-- | A description of the version of a service template.
serviceTemplateVersion_description :: Lens.Lens' ServiceTemplateVersion (Prelude.Maybe Prelude.Text)
serviceTemplateVersion_description = Lens.lens (\ServiceTemplateVersion' {description} -> description) (\s@ServiceTemplateVersion' {} a -> s {description = a} :: ServiceTemplateVersion) Prelude.. Lens.mapping Data._Sensitive

-- | The recommended minor version of the service template.
serviceTemplateVersion_recommendedMinorVersion :: Lens.Lens' ServiceTemplateVersion (Prelude.Maybe Prelude.Text)
serviceTemplateVersion_recommendedMinorVersion = Lens.lens (\ServiceTemplateVersion' {recommendedMinorVersion} -> recommendedMinorVersion) (\s@ServiceTemplateVersion' {} a -> s {recommendedMinorVersion = a} :: ServiceTemplateVersion)

-- | The schema of the version of a service template.
serviceTemplateVersion_schema :: Lens.Lens' ServiceTemplateVersion (Prelude.Maybe Prelude.Text)
serviceTemplateVersion_schema = Lens.lens (\ServiceTemplateVersion' {schema} -> schema) (\s@ServiceTemplateVersion' {} a -> s {schema = a} :: ServiceTemplateVersion) Prelude.. Lens.mapping Data._Sensitive

-- | A service template version status message.
serviceTemplateVersion_statusMessage :: Lens.Lens' ServiceTemplateVersion (Prelude.Maybe Prelude.Text)
serviceTemplateVersion_statusMessage = Lens.lens (\ServiceTemplateVersion' {statusMessage} -> statusMessage) (\s@ServiceTemplateVersion' {} a -> s {statusMessage = a} :: ServiceTemplateVersion) Prelude.. Lens.mapping Data._Sensitive

-- | An array of supported component sources. Components with supported
-- sources can be attached to service instances based on this service
-- template version.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
serviceTemplateVersion_supportedComponentSources :: Lens.Lens' ServiceTemplateVersion (Prelude.Maybe [ServiceTemplateSupportedComponentSourceType])
serviceTemplateVersion_supportedComponentSources = Lens.lens (\ServiceTemplateVersion' {supportedComponentSources} -> supportedComponentSources) (\s@ServiceTemplateVersion' {} a -> s {supportedComponentSources = a} :: ServiceTemplateVersion) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the version of a service template.
serviceTemplateVersion_arn :: Lens.Lens' ServiceTemplateVersion Prelude.Text
serviceTemplateVersion_arn = Lens.lens (\ServiceTemplateVersion' {arn} -> arn) (\s@ServiceTemplateVersion' {} a -> s {arn = a} :: ServiceTemplateVersion)

-- | An array of compatible environment template names for the major version
-- of a service template.
serviceTemplateVersion_compatibleEnvironmentTemplates :: Lens.Lens' ServiceTemplateVersion [CompatibleEnvironmentTemplate]
serviceTemplateVersion_compatibleEnvironmentTemplates = Lens.lens (\ServiceTemplateVersion' {compatibleEnvironmentTemplates} -> compatibleEnvironmentTemplates) (\s@ServiceTemplateVersion' {} a -> s {compatibleEnvironmentTemplates = a} :: ServiceTemplateVersion) Prelude.. Lens.coerced

-- | The time when the version of a service template was created.
serviceTemplateVersion_createdAt :: Lens.Lens' ServiceTemplateVersion Prelude.UTCTime
serviceTemplateVersion_createdAt = Lens.lens (\ServiceTemplateVersion' {createdAt} -> createdAt) (\s@ServiceTemplateVersion' {} a -> s {createdAt = a} :: ServiceTemplateVersion) Prelude.. Data._Time

-- | The time when the version of a service template was last modified.
serviceTemplateVersion_lastModifiedAt :: Lens.Lens' ServiceTemplateVersion Prelude.UTCTime
serviceTemplateVersion_lastModifiedAt = Lens.lens (\ServiceTemplateVersion' {lastModifiedAt} -> lastModifiedAt) (\s@ServiceTemplateVersion' {} a -> s {lastModifiedAt = a} :: ServiceTemplateVersion) Prelude.. Data._Time

-- | The latest major version that\'s associated with the version of a
-- service template.
serviceTemplateVersion_majorVersion :: Lens.Lens' ServiceTemplateVersion Prelude.Text
serviceTemplateVersion_majorVersion = Lens.lens (\ServiceTemplateVersion' {majorVersion} -> majorVersion) (\s@ServiceTemplateVersion' {} a -> s {majorVersion = a} :: ServiceTemplateVersion)

-- | The minor version of a service template.
serviceTemplateVersion_minorVersion :: Lens.Lens' ServiceTemplateVersion Prelude.Text
serviceTemplateVersion_minorVersion = Lens.lens (\ServiceTemplateVersion' {minorVersion} -> minorVersion) (\s@ServiceTemplateVersion' {} a -> s {minorVersion = a} :: ServiceTemplateVersion)

-- | The service template version status.
serviceTemplateVersion_status :: Lens.Lens' ServiceTemplateVersion TemplateVersionStatus
serviceTemplateVersion_status = Lens.lens (\ServiceTemplateVersion' {status} -> status) (\s@ServiceTemplateVersion' {} a -> s {status = a} :: ServiceTemplateVersion)

-- | The name of the version of a service template.
serviceTemplateVersion_templateName :: Lens.Lens' ServiceTemplateVersion Prelude.Text
serviceTemplateVersion_templateName = Lens.lens (\ServiceTemplateVersion' {templateName} -> templateName) (\s@ServiceTemplateVersion' {} a -> s {templateName = a} :: ServiceTemplateVersion)

instance Data.FromJSON ServiceTemplateVersion where
  parseJSON =
    Data.withObject
      "ServiceTemplateVersion"
      ( \x ->
          ServiceTemplateVersion'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "recommendedMinorVersion")
            Prelude.<*> (x Data..:? "schema")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> ( x
                            Data..:? "supportedComponentSources"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> ( x
                            Data..:? "compatibleEnvironmentTemplates"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastModifiedAt")
            Prelude.<*> (x Data..: "majorVersion")
            Prelude.<*> (x Data..: "minorVersion")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "templateName")
      )

instance Prelude.Hashable ServiceTemplateVersion where
  hashWithSalt _salt ServiceTemplateVersion' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` recommendedMinorVersion
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` supportedComponentSources
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` compatibleEnvironmentTemplates
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastModifiedAt
      `Prelude.hashWithSalt` majorVersion
      `Prelude.hashWithSalt` minorVersion
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData ServiceTemplateVersion where
  rnf ServiceTemplateVersion' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf recommendedMinorVersion `Prelude.seq`
        Prelude.rnf schema `Prelude.seq`
          Prelude.rnf statusMessage `Prelude.seq`
            Prelude.rnf supportedComponentSources `Prelude.seq`
              Prelude.rnf arn `Prelude.seq`
                Prelude.rnf compatibleEnvironmentTemplates `Prelude.seq`
                  Prelude.rnf createdAt `Prelude.seq`
                    Prelude.rnf lastModifiedAt `Prelude.seq`
                      Prelude.rnf majorVersion `Prelude.seq`
                        Prelude.rnf minorVersion `Prelude.seq`
                          Prelude.rnf status `Prelude.seq`
                            Prelude.rnf templateName
