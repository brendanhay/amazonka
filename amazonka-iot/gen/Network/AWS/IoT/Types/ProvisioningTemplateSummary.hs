{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A summary of information about a fleet provisioning template.
--
-- /See:/ 'newProvisioningTemplateSummary' smart constructor.
data ProvisioningTemplateSummary = ProvisioningTemplateSummary'
  { -- | The name of the fleet provisioning template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The date when the fleet provisioning template summary was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.POSIX,
    -- | True if the fleet provision template is enabled, otherwise false.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The date when the fleet provisioning template summary was created.
    creationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The description of the fleet provisioning template.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the fleet provisioning template.
    templateArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      enabled = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      description = Prelude.Nothing,
      templateArn = Prelude.Nothing
    }

-- | The name of the fleet provisioning template.
provisioningTemplateSummary_templateName :: Lens.Lens' ProvisioningTemplateSummary (Prelude.Maybe Prelude.Text)
provisioningTemplateSummary_templateName = Lens.lens (\ProvisioningTemplateSummary' {templateName} -> templateName) (\s@ProvisioningTemplateSummary' {} a -> s {templateName = a} :: ProvisioningTemplateSummary)

-- | The date when the fleet provisioning template summary was last modified.
provisioningTemplateSummary_lastModifiedDate :: Lens.Lens' ProvisioningTemplateSummary (Prelude.Maybe Prelude.UTCTime)
provisioningTemplateSummary_lastModifiedDate = Lens.lens (\ProvisioningTemplateSummary' {lastModifiedDate} -> lastModifiedDate) (\s@ProvisioningTemplateSummary' {} a -> s {lastModifiedDate = a} :: ProvisioningTemplateSummary) Prelude.. Lens.mapping Prelude._Time

-- | True if the fleet provision template is enabled, otherwise false.
provisioningTemplateSummary_enabled :: Lens.Lens' ProvisioningTemplateSummary (Prelude.Maybe Prelude.Bool)
provisioningTemplateSummary_enabled = Lens.lens (\ProvisioningTemplateSummary' {enabled} -> enabled) (\s@ProvisioningTemplateSummary' {} a -> s {enabled = a} :: ProvisioningTemplateSummary)

-- | The date when the fleet provisioning template summary was created.
provisioningTemplateSummary_creationDate :: Lens.Lens' ProvisioningTemplateSummary (Prelude.Maybe Prelude.UTCTime)
provisioningTemplateSummary_creationDate = Lens.lens (\ProvisioningTemplateSummary' {creationDate} -> creationDate) (\s@ProvisioningTemplateSummary' {} a -> s {creationDate = a} :: ProvisioningTemplateSummary) Prelude.. Lens.mapping Prelude._Time

-- | The description of the fleet provisioning template.
provisioningTemplateSummary_description :: Lens.Lens' ProvisioningTemplateSummary (Prelude.Maybe Prelude.Text)
provisioningTemplateSummary_description = Lens.lens (\ProvisioningTemplateSummary' {description} -> description) (\s@ProvisioningTemplateSummary' {} a -> s {description = a} :: ProvisioningTemplateSummary)

-- | The ARN of the fleet provisioning template.
provisioningTemplateSummary_templateArn :: Lens.Lens' ProvisioningTemplateSummary (Prelude.Maybe Prelude.Text)
provisioningTemplateSummary_templateArn = Lens.lens (\ProvisioningTemplateSummary' {templateArn} -> templateArn) (\s@ProvisioningTemplateSummary' {} a -> s {templateArn = a} :: ProvisioningTemplateSummary)

instance Prelude.FromJSON ProvisioningTemplateSummary where
  parseJSON =
    Prelude.withObject
      "ProvisioningTemplateSummary"
      ( \x ->
          ProvisioningTemplateSummary'
            Prelude.<$> (x Prelude..:? "templateName")
            Prelude.<*> (x Prelude..:? "lastModifiedDate")
            Prelude.<*> (x Prelude..:? "enabled")
            Prelude.<*> (x Prelude..:? "creationDate")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "templateArn")
      )

instance Prelude.Hashable ProvisioningTemplateSummary

instance Prelude.NFData ProvisioningTemplateSummary
