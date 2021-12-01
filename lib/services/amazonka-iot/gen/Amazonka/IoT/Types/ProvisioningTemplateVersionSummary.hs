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
-- Module      : Amazonka.IoT.Types.ProvisioningTemplateVersionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ProvisioningTemplateVersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A summary of information about a fleet provision template version.
--
-- /See:/ 'newProvisioningTemplateVersionSummary' smart constructor.
data ProvisioningTemplateVersionSummary = ProvisioningTemplateVersionSummary'
  { -- | The ID of the fleet privisioning template version.
    versionId :: Prelude.Maybe Prelude.Int,
    -- | The date when the fleet provisioning template version was created
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | True if the fleet provisioning template version is the default version,
    -- otherwise false.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisioningTemplateVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'provisioningTemplateVersionSummary_versionId' - The ID of the fleet privisioning template version.
--
-- 'creationDate', 'provisioningTemplateVersionSummary_creationDate' - The date when the fleet provisioning template version was created
--
-- 'isDefaultVersion', 'provisioningTemplateVersionSummary_isDefaultVersion' - True if the fleet provisioning template version is the default version,
-- otherwise false.
newProvisioningTemplateVersionSummary ::
  ProvisioningTemplateVersionSummary
newProvisioningTemplateVersionSummary =
  ProvisioningTemplateVersionSummary'
    { versionId =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      isDefaultVersion = Prelude.Nothing
    }

-- | The ID of the fleet privisioning template version.
provisioningTemplateVersionSummary_versionId :: Lens.Lens' ProvisioningTemplateVersionSummary (Prelude.Maybe Prelude.Int)
provisioningTemplateVersionSummary_versionId = Lens.lens (\ProvisioningTemplateVersionSummary' {versionId} -> versionId) (\s@ProvisioningTemplateVersionSummary' {} a -> s {versionId = a} :: ProvisioningTemplateVersionSummary)

-- | The date when the fleet provisioning template version was created
provisioningTemplateVersionSummary_creationDate :: Lens.Lens' ProvisioningTemplateVersionSummary (Prelude.Maybe Prelude.UTCTime)
provisioningTemplateVersionSummary_creationDate = Lens.lens (\ProvisioningTemplateVersionSummary' {creationDate} -> creationDate) (\s@ProvisioningTemplateVersionSummary' {} a -> s {creationDate = a} :: ProvisioningTemplateVersionSummary) Prelude.. Lens.mapping Core._Time

-- | True if the fleet provisioning template version is the default version,
-- otherwise false.
provisioningTemplateVersionSummary_isDefaultVersion :: Lens.Lens' ProvisioningTemplateVersionSummary (Prelude.Maybe Prelude.Bool)
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
            Prelude.<$> (x Core..:? "versionId")
            Prelude.<*> (x Core..:? "creationDate")
            Prelude.<*> (x Core..:? "isDefaultVersion")
      )

instance
  Prelude.Hashable
    ProvisioningTemplateVersionSummary
  where
  hashWithSalt
    salt'
    ProvisioningTemplateVersionSummary' {..} =
      salt' `Prelude.hashWithSalt` isDefaultVersion
        `Prelude.hashWithSalt` creationDate
        `Prelude.hashWithSalt` versionId

instance
  Prelude.NFData
    ProvisioningTemplateVersionSummary
  where
  rnf ProvisioningTemplateVersionSummary' {..} =
    Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf isDefaultVersion
      `Prelude.seq` Prelude.rnf creationDate
