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
-- Module      : Network.AWS.Proton.Types.ServiceTemplateVersionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Proton.Types.ServiceTemplateVersionSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Proton.Types.TemplateVersionStatus

-- | A summary of the service template version detail data.
--
-- /See:/ 'newServiceTemplateVersionSummary' smart constructor.
data ServiceTemplateVersionSummary = ServiceTemplateVersionSummary'
  { -- | A service template minor version status message.
    statusMessage :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of the recommended minor version of the service template.
    recommendedMinorVersion :: Prelude.Maybe Prelude.Text,
    -- | A description of the version of a service template.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the version of a service template.
    arn :: Prelude.Text,
    -- | The time when the version of a service template was created.
    createdAt :: Core.POSIX,
    -- | The time when the version of a service template was last modified.
    lastModifiedAt :: Core.POSIX,
    -- | The ID of the latest major version that\'s associated with the version
    -- of a service template.
    majorVersion :: Prelude.Text,
    -- | The ID of the minor version of a service template.
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
-- 'statusMessage', 'serviceTemplateVersionSummary_statusMessage' - A service template minor version status message.
--
-- 'recommendedMinorVersion', 'serviceTemplateVersionSummary_recommendedMinorVersion' - The ID of the recommended minor version of the service template.
--
-- 'description', 'serviceTemplateVersionSummary_description' - A description of the version of a service template.
--
-- 'arn', 'serviceTemplateVersionSummary_arn' - The Amazon Resource Name (ARN) of the version of a service template.
--
-- 'createdAt', 'serviceTemplateVersionSummary_createdAt' - The time when the version of a service template was created.
--
-- 'lastModifiedAt', 'serviceTemplateVersionSummary_lastModifiedAt' - The time when the version of a service template was last modified.
--
-- 'majorVersion', 'serviceTemplateVersionSummary_majorVersion' - The ID of the latest major version that\'s associated with the version
-- of a service template.
--
-- 'minorVersion', 'serviceTemplateVersionSummary_minorVersion' - The ID of the minor version of a service template.
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
      { statusMessage =
          Prelude.Nothing,
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

-- | A service template minor version status message.
serviceTemplateVersionSummary_statusMessage :: Lens.Lens' ServiceTemplateVersionSummary (Prelude.Maybe Prelude.Text)
serviceTemplateVersionSummary_statusMessage = Lens.lens (\ServiceTemplateVersionSummary' {statusMessage} -> statusMessage) (\s@ServiceTemplateVersionSummary' {} a -> s {statusMessage = a} :: ServiceTemplateVersionSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of the recommended minor version of the service template.
serviceTemplateVersionSummary_recommendedMinorVersion :: Lens.Lens' ServiceTemplateVersionSummary (Prelude.Maybe Prelude.Text)
serviceTemplateVersionSummary_recommendedMinorVersion = Lens.lens (\ServiceTemplateVersionSummary' {recommendedMinorVersion} -> recommendedMinorVersion) (\s@ServiceTemplateVersionSummary' {} a -> s {recommendedMinorVersion = a} :: ServiceTemplateVersionSummary)

-- | A description of the version of a service template.
serviceTemplateVersionSummary_description :: Lens.Lens' ServiceTemplateVersionSummary (Prelude.Maybe Prelude.Text)
serviceTemplateVersionSummary_description = Lens.lens (\ServiceTemplateVersionSummary' {description} -> description) (\s@ServiceTemplateVersionSummary' {} a -> s {description = a} :: ServiceTemplateVersionSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) of the version of a service template.
serviceTemplateVersionSummary_arn :: Lens.Lens' ServiceTemplateVersionSummary Prelude.Text
serviceTemplateVersionSummary_arn = Lens.lens (\ServiceTemplateVersionSummary' {arn} -> arn) (\s@ServiceTemplateVersionSummary' {} a -> s {arn = a} :: ServiceTemplateVersionSummary)

-- | The time when the version of a service template was created.
serviceTemplateVersionSummary_createdAt :: Lens.Lens' ServiceTemplateVersionSummary Prelude.UTCTime
serviceTemplateVersionSummary_createdAt = Lens.lens (\ServiceTemplateVersionSummary' {createdAt} -> createdAt) (\s@ServiceTemplateVersionSummary' {} a -> s {createdAt = a} :: ServiceTemplateVersionSummary) Prelude.. Core._Time

-- | The time when the version of a service template was last modified.
serviceTemplateVersionSummary_lastModifiedAt :: Lens.Lens' ServiceTemplateVersionSummary Prelude.UTCTime
serviceTemplateVersionSummary_lastModifiedAt = Lens.lens (\ServiceTemplateVersionSummary' {lastModifiedAt} -> lastModifiedAt) (\s@ServiceTemplateVersionSummary' {} a -> s {lastModifiedAt = a} :: ServiceTemplateVersionSummary) Prelude.. Core._Time

-- | The ID of the latest major version that\'s associated with the version
-- of a service template.
serviceTemplateVersionSummary_majorVersion :: Lens.Lens' ServiceTemplateVersionSummary Prelude.Text
serviceTemplateVersionSummary_majorVersion = Lens.lens (\ServiceTemplateVersionSummary' {majorVersion} -> majorVersion) (\s@ServiceTemplateVersionSummary' {} a -> s {majorVersion = a} :: ServiceTemplateVersionSummary)

-- | The ID of the minor version of a service template.
serviceTemplateVersionSummary_minorVersion :: Lens.Lens' ServiceTemplateVersionSummary Prelude.Text
serviceTemplateVersionSummary_minorVersion = Lens.lens (\ServiceTemplateVersionSummary' {minorVersion} -> minorVersion) (\s@ServiceTemplateVersionSummary' {} a -> s {minorVersion = a} :: ServiceTemplateVersionSummary)

-- | The service template minor version status.
serviceTemplateVersionSummary_status :: Lens.Lens' ServiceTemplateVersionSummary TemplateVersionStatus
serviceTemplateVersionSummary_status = Lens.lens (\ServiceTemplateVersionSummary' {status} -> status) (\s@ServiceTemplateVersionSummary' {} a -> s {status = a} :: ServiceTemplateVersionSummary)

-- | The name of the service template.
serviceTemplateVersionSummary_templateName :: Lens.Lens' ServiceTemplateVersionSummary Prelude.Text
serviceTemplateVersionSummary_templateName = Lens.lens (\ServiceTemplateVersionSummary' {templateName} -> templateName) (\s@ServiceTemplateVersionSummary' {} a -> s {templateName = a} :: ServiceTemplateVersionSummary)

instance Core.FromJSON ServiceTemplateVersionSummary where
  parseJSON =
    Core.withObject
      "ServiceTemplateVersionSummary"
      ( \x ->
          ServiceTemplateVersionSummary'
            Prelude.<$> (x Core..:? "statusMessage")
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

instance
  Prelude.Hashable
    ServiceTemplateVersionSummary

instance Prelude.NFData ServiceTemplateVersionSummary
