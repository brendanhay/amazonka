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
-- Module      : Amazonka.AuditManager.Types.Settings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.Settings where

import Amazonka.AuditManager.Types.AssessmentReportsDestination
import Amazonka.AuditManager.Types.DeregistrationPolicy
import Amazonka.AuditManager.Types.EvidenceFinderEnablement
import Amazonka.AuditManager.Types.Role
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The settings object that holds all supported Audit Manager settings.
--
-- /See:/ 'newSettings' smart constructor.
data Settings = Settings'
  { -- | The default storage destination for assessment reports.
    defaultAssessmentReportsDestination :: Prelude.Maybe AssessmentReportsDestination,
    -- | The designated default audit owners.
    defaultProcessOwners :: Prelude.Maybe [Role],
    -- | The deregistration policy for your Audit Manager data. You can use this
    -- attribute to determine how your data is handled when you deregister
    -- Audit Manager.
    deregistrationPolicy :: Prelude.Maybe DeregistrationPolicy,
    -- | The current evidence finder status and event data store details.
    evidenceFinderEnablement :: Prelude.Maybe EvidenceFinderEnablement,
    -- | Specifies whether Organizations is enabled.
    isAwsOrgEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The KMS key details.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The designated Amazon Simple Notification Service (Amazon SNS) topic.
    snsTopic :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultAssessmentReportsDestination', 'settings_defaultAssessmentReportsDestination' - The default storage destination for assessment reports.
--
-- 'defaultProcessOwners', 'settings_defaultProcessOwners' - The designated default audit owners.
--
-- 'deregistrationPolicy', 'settings_deregistrationPolicy' - The deregistration policy for your Audit Manager data. You can use this
-- attribute to determine how your data is handled when you deregister
-- Audit Manager.
--
-- 'evidenceFinderEnablement', 'settings_evidenceFinderEnablement' - The current evidence finder status and event data store details.
--
-- 'isAwsOrgEnabled', 'settings_isAwsOrgEnabled' - Specifies whether Organizations is enabled.
--
-- 'kmsKey', 'settings_kmsKey' - The KMS key details.
--
-- 'snsTopic', 'settings_snsTopic' - The designated Amazon Simple Notification Service (Amazon SNS) topic.
newSettings ::
  Settings
newSettings =
  Settings'
    { defaultAssessmentReportsDestination =
        Prelude.Nothing,
      defaultProcessOwners = Prelude.Nothing,
      deregistrationPolicy = Prelude.Nothing,
      evidenceFinderEnablement = Prelude.Nothing,
      isAwsOrgEnabled = Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      snsTopic = Prelude.Nothing
    }

-- | The default storage destination for assessment reports.
settings_defaultAssessmentReportsDestination :: Lens.Lens' Settings (Prelude.Maybe AssessmentReportsDestination)
settings_defaultAssessmentReportsDestination = Lens.lens (\Settings' {defaultAssessmentReportsDestination} -> defaultAssessmentReportsDestination) (\s@Settings' {} a -> s {defaultAssessmentReportsDestination = a} :: Settings)

-- | The designated default audit owners.
settings_defaultProcessOwners :: Lens.Lens' Settings (Prelude.Maybe [Role])
settings_defaultProcessOwners = Lens.lens (\Settings' {defaultProcessOwners} -> defaultProcessOwners) (\s@Settings' {} a -> s {defaultProcessOwners = a} :: Settings) Prelude.. Lens.mapping Lens.coerced

-- | The deregistration policy for your Audit Manager data. You can use this
-- attribute to determine how your data is handled when you deregister
-- Audit Manager.
settings_deregistrationPolicy :: Lens.Lens' Settings (Prelude.Maybe DeregistrationPolicy)
settings_deregistrationPolicy = Lens.lens (\Settings' {deregistrationPolicy} -> deregistrationPolicy) (\s@Settings' {} a -> s {deregistrationPolicy = a} :: Settings)

-- | The current evidence finder status and event data store details.
settings_evidenceFinderEnablement :: Lens.Lens' Settings (Prelude.Maybe EvidenceFinderEnablement)
settings_evidenceFinderEnablement = Lens.lens (\Settings' {evidenceFinderEnablement} -> evidenceFinderEnablement) (\s@Settings' {} a -> s {evidenceFinderEnablement = a} :: Settings)

-- | Specifies whether Organizations is enabled.
settings_isAwsOrgEnabled :: Lens.Lens' Settings (Prelude.Maybe Prelude.Bool)
settings_isAwsOrgEnabled = Lens.lens (\Settings' {isAwsOrgEnabled} -> isAwsOrgEnabled) (\s@Settings' {} a -> s {isAwsOrgEnabled = a} :: Settings)

-- | The KMS key details.
settings_kmsKey :: Lens.Lens' Settings (Prelude.Maybe Prelude.Text)
settings_kmsKey = Lens.lens (\Settings' {kmsKey} -> kmsKey) (\s@Settings' {} a -> s {kmsKey = a} :: Settings)

-- | The designated Amazon Simple Notification Service (Amazon SNS) topic.
settings_snsTopic :: Lens.Lens' Settings (Prelude.Maybe Prelude.Text)
settings_snsTopic = Lens.lens (\Settings' {snsTopic} -> snsTopic) (\s@Settings' {} a -> s {snsTopic = a} :: Settings)

instance Data.FromJSON Settings where
  parseJSON =
    Data.withObject
      "Settings"
      ( \x ->
          Settings'
            Prelude.<$> (x Data..:? "defaultAssessmentReportsDestination")
            Prelude.<*> ( x
                            Data..:? "defaultProcessOwners"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "deregistrationPolicy")
            Prelude.<*> (x Data..:? "evidenceFinderEnablement")
            Prelude.<*> (x Data..:? "isAwsOrgEnabled")
            Prelude.<*> (x Data..:? "kmsKey")
            Prelude.<*> (x Data..:? "snsTopic")
      )

instance Prelude.Hashable Settings where
  hashWithSalt _salt Settings' {..} =
    _salt
      `Prelude.hashWithSalt` defaultAssessmentReportsDestination
      `Prelude.hashWithSalt` defaultProcessOwners
      `Prelude.hashWithSalt` deregistrationPolicy
      `Prelude.hashWithSalt` evidenceFinderEnablement
      `Prelude.hashWithSalt` isAwsOrgEnabled
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` snsTopic

instance Prelude.NFData Settings where
  rnf Settings' {..} =
    Prelude.rnf defaultAssessmentReportsDestination
      `Prelude.seq` Prelude.rnf defaultProcessOwners
      `Prelude.seq` Prelude.rnf deregistrationPolicy
      `Prelude.seq` Prelude.rnf evidenceFinderEnablement
      `Prelude.seq` Prelude.rnf isAwsOrgEnabled
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf snsTopic
