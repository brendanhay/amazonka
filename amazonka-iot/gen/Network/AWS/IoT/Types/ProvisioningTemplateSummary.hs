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
-- Module      : Network.AWS.IoT.Types.ProvisioningTemplateSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ProvisioningTemplateSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A summary of information about a fleet provisioning template.
--
-- /See:/ 'newProvisioningTemplateSummary' smart constructor.
data ProvisioningTemplateSummary = ProvisioningTemplateSummary'
  { -- | The name of the fleet provisioning template.
    templateName :: Core.Maybe Core.Text,
    -- | The date when the fleet provisioning template summary was last modified.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | True if the fleet provision template is enabled, otherwise false.
    enabled :: Core.Maybe Core.Bool,
    -- | The date when the fleet provisioning template summary was created.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The description of the fleet provisioning template.
    description :: Core.Maybe Core.Text,
    -- | The ARN of the fleet provisioning template.
    templateArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvisioningTemplateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'provisioningTemplateSummary_templateName' - The name of the fleet provisioning template.
--
-- 'lastModifiedDate', 'provisioningTemplateSummary_lastModifiedDate' - The date when the fleet provisioning template summary was last modified.
--
-- 'enabled', 'provisioningTemplateSummary_enabled' - True if the fleet provision template is enabled, otherwise false.
--
-- 'creationDate', 'provisioningTemplateSummary_creationDate' - The date when the fleet provisioning template summary was created.
--
-- 'description', 'provisioningTemplateSummary_description' - The description of the fleet provisioning template.
--
-- 'templateArn', 'provisioningTemplateSummary_templateArn' - The ARN of the fleet provisioning template.
newProvisioningTemplateSummary ::
  ProvisioningTemplateSummary
newProvisioningTemplateSummary =
  ProvisioningTemplateSummary'
    { templateName =
        Core.Nothing,
      lastModifiedDate = Core.Nothing,
      enabled = Core.Nothing,
      creationDate = Core.Nothing,
      description = Core.Nothing,
      templateArn = Core.Nothing
    }

-- | The name of the fleet provisioning template.
provisioningTemplateSummary_templateName :: Lens.Lens' ProvisioningTemplateSummary (Core.Maybe Core.Text)
provisioningTemplateSummary_templateName = Lens.lens (\ProvisioningTemplateSummary' {templateName} -> templateName) (\s@ProvisioningTemplateSummary' {} a -> s {templateName = a} :: ProvisioningTemplateSummary)

-- | The date when the fleet provisioning template summary was last modified.
provisioningTemplateSummary_lastModifiedDate :: Lens.Lens' ProvisioningTemplateSummary (Core.Maybe Core.UTCTime)
provisioningTemplateSummary_lastModifiedDate = Lens.lens (\ProvisioningTemplateSummary' {lastModifiedDate} -> lastModifiedDate) (\s@ProvisioningTemplateSummary' {} a -> s {lastModifiedDate = a} :: ProvisioningTemplateSummary) Core.. Lens.mapping Core._Time

-- | True if the fleet provision template is enabled, otherwise false.
provisioningTemplateSummary_enabled :: Lens.Lens' ProvisioningTemplateSummary (Core.Maybe Core.Bool)
provisioningTemplateSummary_enabled = Lens.lens (\ProvisioningTemplateSummary' {enabled} -> enabled) (\s@ProvisioningTemplateSummary' {} a -> s {enabled = a} :: ProvisioningTemplateSummary)

-- | The date when the fleet provisioning template summary was created.
provisioningTemplateSummary_creationDate :: Lens.Lens' ProvisioningTemplateSummary (Core.Maybe Core.UTCTime)
provisioningTemplateSummary_creationDate = Lens.lens (\ProvisioningTemplateSummary' {creationDate} -> creationDate) (\s@ProvisioningTemplateSummary' {} a -> s {creationDate = a} :: ProvisioningTemplateSummary) Core.. Lens.mapping Core._Time

-- | The description of the fleet provisioning template.
provisioningTemplateSummary_description :: Lens.Lens' ProvisioningTemplateSummary (Core.Maybe Core.Text)
provisioningTemplateSummary_description = Lens.lens (\ProvisioningTemplateSummary' {description} -> description) (\s@ProvisioningTemplateSummary' {} a -> s {description = a} :: ProvisioningTemplateSummary)

-- | The ARN of the fleet provisioning template.
provisioningTemplateSummary_templateArn :: Lens.Lens' ProvisioningTemplateSummary (Core.Maybe Core.Text)
provisioningTemplateSummary_templateArn = Lens.lens (\ProvisioningTemplateSummary' {templateArn} -> templateArn) (\s@ProvisioningTemplateSummary' {} a -> s {templateArn = a} :: ProvisioningTemplateSummary)

instance Core.FromJSON ProvisioningTemplateSummary where
  parseJSON =
    Core.withObject
      "ProvisioningTemplateSummary"
      ( \x ->
          ProvisioningTemplateSummary'
            Core.<$> (x Core..:? "templateName")
            Core.<*> (x Core..:? "lastModifiedDate")
            Core.<*> (x Core..:? "enabled")
            Core.<*> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "templateArn")
      )

instance Core.Hashable ProvisioningTemplateSummary

instance Core.NFData ProvisioningTemplateSummary
