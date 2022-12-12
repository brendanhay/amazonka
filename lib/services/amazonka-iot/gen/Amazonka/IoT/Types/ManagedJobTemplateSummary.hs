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
-- Module      : Amazonka.IoT.Types.ManagedJobTemplateSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ManagedJobTemplateSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about the managed template.
--
-- /See:/ 'newManagedJobTemplateSummary' smart constructor.
data ManagedJobTemplateSummary = ManagedJobTemplateSummary'
  { -- | The description for a managed template.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of environments that are supported with the managed job template.
    environments :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) for a managed template.
    templateArn :: Prelude.Maybe Prelude.Text,
    -- | The unique Name for a managed template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The version for a managed template.
    templateVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedJobTemplateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'managedJobTemplateSummary_description' - The description for a managed template.
--
-- 'environments', 'managedJobTemplateSummary_environments' - A list of environments that are supported with the managed job template.
--
-- 'templateArn', 'managedJobTemplateSummary_templateArn' - The Amazon Resource Name (ARN) for a managed template.
--
-- 'templateName', 'managedJobTemplateSummary_templateName' - The unique Name for a managed template.
--
-- 'templateVersion', 'managedJobTemplateSummary_templateVersion' - The version for a managed template.
newManagedJobTemplateSummary ::
  ManagedJobTemplateSummary
newManagedJobTemplateSummary =
  ManagedJobTemplateSummary'
    { description =
        Prelude.Nothing,
      environments = Prelude.Nothing,
      templateArn = Prelude.Nothing,
      templateName = Prelude.Nothing,
      templateVersion = Prelude.Nothing
    }

-- | The description for a managed template.
managedJobTemplateSummary_description :: Lens.Lens' ManagedJobTemplateSummary (Prelude.Maybe Prelude.Text)
managedJobTemplateSummary_description = Lens.lens (\ManagedJobTemplateSummary' {description} -> description) (\s@ManagedJobTemplateSummary' {} a -> s {description = a} :: ManagedJobTemplateSummary)

-- | A list of environments that are supported with the managed job template.
managedJobTemplateSummary_environments :: Lens.Lens' ManagedJobTemplateSummary (Prelude.Maybe [Prelude.Text])
managedJobTemplateSummary_environments = Lens.lens (\ManagedJobTemplateSummary' {environments} -> environments) (\s@ManagedJobTemplateSummary' {} a -> s {environments = a} :: ManagedJobTemplateSummary) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for a managed template.
managedJobTemplateSummary_templateArn :: Lens.Lens' ManagedJobTemplateSummary (Prelude.Maybe Prelude.Text)
managedJobTemplateSummary_templateArn = Lens.lens (\ManagedJobTemplateSummary' {templateArn} -> templateArn) (\s@ManagedJobTemplateSummary' {} a -> s {templateArn = a} :: ManagedJobTemplateSummary)

-- | The unique Name for a managed template.
managedJobTemplateSummary_templateName :: Lens.Lens' ManagedJobTemplateSummary (Prelude.Maybe Prelude.Text)
managedJobTemplateSummary_templateName = Lens.lens (\ManagedJobTemplateSummary' {templateName} -> templateName) (\s@ManagedJobTemplateSummary' {} a -> s {templateName = a} :: ManagedJobTemplateSummary)

-- | The version for a managed template.
managedJobTemplateSummary_templateVersion :: Lens.Lens' ManagedJobTemplateSummary (Prelude.Maybe Prelude.Text)
managedJobTemplateSummary_templateVersion = Lens.lens (\ManagedJobTemplateSummary' {templateVersion} -> templateVersion) (\s@ManagedJobTemplateSummary' {} a -> s {templateVersion = a} :: ManagedJobTemplateSummary)

instance Data.FromJSON ManagedJobTemplateSummary where
  parseJSON =
    Data.withObject
      "ManagedJobTemplateSummary"
      ( \x ->
          ManagedJobTemplateSummary'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "environments" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "templateArn")
            Prelude.<*> (x Data..:? "templateName")
            Prelude.<*> (x Data..:? "templateVersion")
      )

instance Prelude.Hashable ManagedJobTemplateSummary where
  hashWithSalt _salt ManagedJobTemplateSummary' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` environments
      `Prelude.hashWithSalt` templateArn
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateVersion

instance Prelude.NFData ManagedJobTemplateSummary where
  rnf ManagedJobTemplateSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf environments
      `Prelude.seq` Prelude.rnf templateArn
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateVersion
