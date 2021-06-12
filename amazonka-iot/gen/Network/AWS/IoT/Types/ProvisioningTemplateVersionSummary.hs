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
-- Module      : Network.AWS.IoT.Types.ProvisioningTemplateVersionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ProvisioningTemplateVersionSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A summary of information about a fleet provision template version.
--
-- /See:/ 'newProvisioningTemplateVersionSummary' smart constructor.
data ProvisioningTemplateVersionSummary = ProvisioningTemplateVersionSummary'
  { -- | The date when the fleet provisioning template version was created
    creationDate :: Core.Maybe Core.POSIX,
    -- | The ID of the fleet privisioning template version.
    versionId :: Core.Maybe Core.Int,
    -- | True if the fleet provisioning template version is the default version,
    -- otherwise false.
    isDefaultVersion :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvisioningTemplateVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'provisioningTemplateVersionSummary_creationDate' - The date when the fleet provisioning template version was created
--
-- 'versionId', 'provisioningTemplateVersionSummary_versionId' - The ID of the fleet privisioning template version.
--
-- 'isDefaultVersion', 'provisioningTemplateVersionSummary_isDefaultVersion' - True if the fleet provisioning template version is the default version,
-- otherwise false.
newProvisioningTemplateVersionSummary ::
  ProvisioningTemplateVersionSummary
newProvisioningTemplateVersionSummary =
  ProvisioningTemplateVersionSummary'
    { creationDate =
        Core.Nothing,
      versionId = Core.Nothing,
      isDefaultVersion = Core.Nothing
    }

-- | The date when the fleet provisioning template version was created
provisioningTemplateVersionSummary_creationDate :: Lens.Lens' ProvisioningTemplateVersionSummary (Core.Maybe Core.UTCTime)
provisioningTemplateVersionSummary_creationDate = Lens.lens (\ProvisioningTemplateVersionSummary' {creationDate} -> creationDate) (\s@ProvisioningTemplateVersionSummary' {} a -> s {creationDate = a} :: ProvisioningTemplateVersionSummary) Core.. Lens.mapping Core._Time

-- | The ID of the fleet privisioning template version.
provisioningTemplateVersionSummary_versionId :: Lens.Lens' ProvisioningTemplateVersionSummary (Core.Maybe Core.Int)
provisioningTemplateVersionSummary_versionId = Lens.lens (\ProvisioningTemplateVersionSummary' {versionId} -> versionId) (\s@ProvisioningTemplateVersionSummary' {} a -> s {versionId = a} :: ProvisioningTemplateVersionSummary)

-- | True if the fleet provisioning template version is the default version,
-- otherwise false.
provisioningTemplateVersionSummary_isDefaultVersion :: Lens.Lens' ProvisioningTemplateVersionSummary (Core.Maybe Core.Bool)
provisioningTemplateVersionSummary_isDefaultVersion = Lens.lens (\ProvisioningTemplateVersionSummary' {isDefaultVersion} -> isDefaultVersion) (\s@ProvisioningTemplateVersionSummary' {} a -> s {isDefaultVersion = a} :: ProvisioningTemplateVersionSummary)

instance
  Core.FromJSON
    ProvisioningTemplateVersionSummary
  where
  parseJSON =
    Core.withObject
      "ProvisioningTemplateVersionSummary"
      ( \x ->
          ProvisioningTemplateVersionSummary'
            Core.<$> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "versionId")
            Core.<*> (x Core..:? "isDefaultVersion")
      )

instance
  Core.Hashable
    ProvisioningTemplateVersionSummary

instance
  Core.NFData
    ProvisioningTemplateVersionSummary
