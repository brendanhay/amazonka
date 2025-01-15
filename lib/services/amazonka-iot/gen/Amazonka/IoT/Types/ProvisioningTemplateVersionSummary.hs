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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ProvisioningTemplateVersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of information about a fleet provision template version.
--
-- /See:/ 'newProvisioningTemplateVersionSummary' smart constructor.
data ProvisioningTemplateVersionSummary = ProvisioningTemplateVersionSummary'
  { -- | The date when the provisioning template version was created
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | True if the provisioning template version is the default version,
    -- otherwise false.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the fleet provisioning template version.
    versionId :: Prelude.Maybe Prelude.Int
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
-- 'creationDate', 'provisioningTemplateVersionSummary_creationDate' - The date when the provisioning template version was created
--
-- 'isDefaultVersion', 'provisioningTemplateVersionSummary_isDefaultVersion' - True if the provisioning template version is the default version,
-- otherwise false.
--
-- 'versionId', 'provisioningTemplateVersionSummary_versionId' - The ID of the fleet provisioning template version.
newProvisioningTemplateVersionSummary ::
  ProvisioningTemplateVersionSummary
newProvisioningTemplateVersionSummary =
  ProvisioningTemplateVersionSummary'
    { creationDate =
        Prelude.Nothing,
      isDefaultVersion = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | The date when the provisioning template version was created
provisioningTemplateVersionSummary_creationDate :: Lens.Lens' ProvisioningTemplateVersionSummary (Prelude.Maybe Prelude.UTCTime)
provisioningTemplateVersionSummary_creationDate = Lens.lens (\ProvisioningTemplateVersionSummary' {creationDate} -> creationDate) (\s@ProvisioningTemplateVersionSummary' {} a -> s {creationDate = a} :: ProvisioningTemplateVersionSummary) Prelude.. Lens.mapping Data._Time

-- | True if the provisioning template version is the default version,
-- otherwise false.
provisioningTemplateVersionSummary_isDefaultVersion :: Lens.Lens' ProvisioningTemplateVersionSummary (Prelude.Maybe Prelude.Bool)
provisioningTemplateVersionSummary_isDefaultVersion = Lens.lens (\ProvisioningTemplateVersionSummary' {isDefaultVersion} -> isDefaultVersion) (\s@ProvisioningTemplateVersionSummary' {} a -> s {isDefaultVersion = a} :: ProvisioningTemplateVersionSummary)

-- | The ID of the fleet provisioning template version.
provisioningTemplateVersionSummary_versionId :: Lens.Lens' ProvisioningTemplateVersionSummary (Prelude.Maybe Prelude.Int)
provisioningTemplateVersionSummary_versionId = Lens.lens (\ProvisioningTemplateVersionSummary' {versionId} -> versionId) (\s@ProvisioningTemplateVersionSummary' {} a -> s {versionId = a} :: ProvisioningTemplateVersionSummary)

instance
  Data.FromJSON
    ProvisioningTemplateVersionSummary
  where
  parseJSON =
    Data.withObject
      "ProvisioningTemplateVersionSummary"
      ( \x ->
          ProvisioningTemplateVersionSummary'
            Prelude.<$> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "isDefaultVersion")
            Prelude.<*> (x Data..:? "versionId")
      )

instance
  Prelude.Hashable
    ProvisioningTemplateVersionSummary
  where
  hashWithSalt
    _salt
    ProvisioningTemplateVersionSummary' {..} =
      _salt
        `Prelude.hashWithSalt` creationDate
        `Prelude.hashWithSalt` isDefaultVersion
        `Prelude.hashWithSalt` versionId

instance
  Prelude.NFData
    ProvisioningTemplateVersionSummary
  where
  rnf ProvisioningTemplateVersionSummary' {..} =
    Prelude.rnf creationDate `Prelude.seq`
      Prelude.rnf isDefaultVersion `Prelude.seq`
        Prelude.rnf versionId
