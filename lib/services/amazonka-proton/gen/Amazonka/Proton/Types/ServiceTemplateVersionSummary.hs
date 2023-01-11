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
-- Module      : Amazonka.Proton.Types.ServiceTemplateVersionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ServiceTemplateVersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.TemplateVersionStatus

-- | Summary data of an Proton service template version resource.
--
-- /See:/ 'newServiceTemplateVersionSummary' smart constructor.
data ServiceTemplateVersionSummary = ServiceTemplateVersionSummary'
  { -- | A description of the version of a service template.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The recommended minor version of the service template.
    recommendedMinorVersion :: Prelude.Maybe Prelude.Text,
    -- | A service template minor version status message.
    statusMessage :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the version of a service template.
    arn :: Prelude.Text,
    -- | The time when the version of a service template was created.
    createdAt :: Data.POSIX,
    -- | The time when the version of a service template was last modified.
    lastModifiedAt :: Data.POSIX,
    -- | The latest major version that\'s associated with the version of a
    -- service template.
    majorVersion :: Prelude.Text,
    -- | The minor version of a service template.
    minorVersion :: Prelude.Text,
    -- | The service template minor version status.
    status :: TemplateVersionStatus,
    -- | The name of the service template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceTemplateVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'serviceTemplateVersionSummary_description' - A description of the version of a service template.
--
-- 'recommendedMinorVersion', 'serviceTemplateVersionSummary_recommendedMinorVersion' - The recommended minor version of the service template.
--
-- 'statusMessage', 'serviceTemplateVersionSummary_statusMessage' - A service template minor version status message.
--
-- 'arn', 'serviceTemplateVersionSummary_arn' - The Amazon Resource Name (ARN) of the version of a service template.
--
-- 'createdAt', 'serviceTemplateVersionSummary_createdAt' - The time when the version of a service template was created.
--
-- 'lastModifiedAt', 'serviceTemplateVersionSummary_lastModifiedAt' - The time when the version of a service template was last modified.
--
-- 'majorVersion', 'serviceTemplateVersionSummary_majorVersion' - The latest major version that\'s associated with the version of a
-- service template.
--
-- 'minorVersion', 'serviceTemplateVersionSummary_minorVersion' - The minor version of a service template.
--
-- 'status', 'serviceTemplateVersionSummary_status' - The service template minor version status.
--
-- 'templateName', 'serviceTemplateVersionSummary_templateName' - The name of the service template.
newServiceTemplateVersionSummary ::
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
  ServiceTemplateVersionSummary
newServiceTemplateVersionSummary
  pArn_
  pCreatedAt_
  pLastModifiedAt_
  pMajorVersion_
  pMinorVersion_
  pStatus_
  pTemplateName_ =
    ServiceTemplateVersionSummary'
      { description =
          Prelude.Nothing,
        recommendedMinorVersion = Prelude.Nothing,
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

-- | A description of the version of a service template.
serviceTemplateVersionSummary_description :: Lens.Lens' ServiceTemplateVersionSummary (Prelude.Maybe Prelude.Text)
serviceTemplateVersionSummary_description = Lens.lens (\ServiceTemplateVersionSummary' {description} -> description) (\s@ServiceTemplateVersionSummary' {} a -> s {description = a} :: ServiceTemplateVersionSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The recommended minor version of the service template.
serviceTemplateVersionSummary_recommendedMinorVersion :: Lens.Lens' ServiceTemplateVersionSummary (Prelude.Maybe Prelude.Text)
serviceTemplateVersionSummary_recommendedMinorVersion = Lens.lens (\ServiceTemplateVersionSummary' {recommendedMinorVersion} -> recommendedMinorVersion) (\s@ServiceTemplateVersionSummary' {} a -> s {recommendedMinorVersion = a} :: ServiceTemplateVersionSummary)

-- | A service template minor version status message.
serviceTemplateVersionSummary_statusMessage :: Lens.Lens' ServiceTemplateVersionSummary (Prelude.Maybe Prelude.Text)
serviceTemplateVersionSummary_statusMessage = Lens.lens (\ServiceTemplateVersionSummary' {statusMessage} -> statusMessage) (\s@ServiceTemplateVersionSummary' {} a -> s {statusMessage = a} :: ServiceTemplateVersionSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) of the version of a service template.
serviceTemplateVersionSummary_arn :: Lens.Lens' ServiceTemplateVersionSummary Prelude.Text
serviceTemplateVersionSummary_arn = Lens.lens (\ServiceTemplateVersionSummary' {arn} -> arn) (\s@ServiceTemplateVersionSummary' {} a -> s {arn = a} :: ServiceTemplateVersionSummary)

-- | The time when the version of a service template was created.
serviceTemplateVersionSummary_createdAt :: Lens.Lens' ServiceTemplateVersionSummary Prelude.UTCTime
serviceTemplateVersionSummary_createdAt = Lens.lens (\ServiceTemplateVersionSummary' {createdAt} -> createdAt) (\s@ServiceTemplateVersionSummary' {} a -> s {createdAt = a} :: ServiceTemplateVersionSummary) Prelude.. Data._Time

-- | The time when the version of a service template was last modified.
serviceTemplateVersionSummary_lastModifiedAt :: Lens.Lens' ServiceTemplateVersionSummary Prelude.UTCTime
serviceTemplateVersionSummary_lastModifiedAt = Lens.lens (\ServiceTemplateVersionSummary' {lastModifiedAt} -> lastModifiedAt) (\s@ServiceTemplateVersionSummary' {} a -> s {lastModifiedAt = a} :: ServiceTemplateVersionSummary) Prelude.. Data._Time

-- | The latest major version that\'s associated with the version of a
-- service template.
serviceTemplateVersionSummary_majorVersion :: Lens.Lens' ServiceTemplateVersionSummary Prelude.Text
serviceTemplateVersionSummary_majorVersion = Lens.lens (\ServiceTemplateVersionSummary' {majorVersion} -> majorVersion) (\s@ServiceTemplateVersionSummary' {} a -> s {majorVersion = a} :: ServiceTemplateVersionSummary)

-- | The minor version of a service template.
serviceTemplateVersionSummary_minorVersion :: Lens.Lens' ServiceTemplateVersionSummary Prelude.Text
serviceTemplateVersionSummary_minorVersion = Lens.lens (\ServiceTemplateVersionSummary' {minorVersion} -> minorVersion) (\s@ServiceTemplateVersionSummary' {} a -> s {minorVersion = a} :: ServiceTemplateVersionSummary)

-- | The service template minor version status.
serviceTemplateVersionSummary_status :: Lens.Lens' ServiceTemplateVersionSummary TemplateVersionStatus
serviceTemplateVersionSummary_status = Lens.lens (\ServiceTemplateVersionSummary' {status} -> status) (\s@ServiceTemplateVersionSummary' {} a -> s {status = a} :: ServiceTemplateVersionSummary)

-- | The name of the service template.
serviceTemplateVersionSummary_templateName :: Lens.Lens' ServiceTemplateVersionSummary Prelude.Text
serviceTemplateVersionSummary_templateName = Lens.lens (\ServiceTemplateVersionSummary' {templateName} -> templateName) (\s@ServiceTemplateVersionSummary' {} a -> s {templateName = a} :: ServiceTemplateVersionSummary)

instance Data.FromJSON ServiceTemplateVersionSummary where
  parseJSON =
    Data.withObject
      "ServiceTemplateVersionSummary"
      ( \x ->
          ServiceTemplateVersionSummary'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "recommendedMinorVersion")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "lastModifiedAt")
            Prelude.<*> (x Data..: "majorVersion")
            Prelude.<*> (x Data..: "minorVersion")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "templateName")
      )

instance
  Prelude.Hashable
    ServiceTemplateVersionSummary
  where
  hashWithSalt _salt ServiceTemplateVersionSummary' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` recommendedMinorVersion
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastModifiedAt
      `Prelude.hashWithSalt` majorVersion
      `Prelude.hashWithSalt` minorVersion
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData ServiceTemplateVersionSummary where
  rnf ServiceTemplateVersionSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf recommendedMinorVersion
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModifiedAt
      `Prelude.seq` Prelude.rnf majorVersion
      `Prelude.seq` Prelude.rnf minorVersion
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf templateName
