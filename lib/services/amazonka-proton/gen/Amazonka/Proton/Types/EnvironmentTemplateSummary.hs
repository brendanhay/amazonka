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
-- Module      : Amazonka.Proton.Types.EnvironmentTemplateSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.EnvironmentTemplateSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.Provisioning

-- | The environment template data.
--
-- /See:/ 'newEnvironmentTemplateSummary' smart constructor.
data EnvironmentTemplateSummary = EnvironmentTemplateSummary'
  { -- | The recommended version of the environment template.
    recommendedVersion :: Prelude.Maybe Prelude.Text,
    -- | When included, indicates that the environment template is for customer
    -- provisioned and managed infrastructure.
    provisioning :: Prelude.Maybe Provisioning,
    -- | The name of the environment template as displayed in the developer
    -- interface.
    displayName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A description of the environment template.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the environment template.
    arn :: Prelude.Text,
    -- | The time when the environment template was created.
    createdAt :: Core.POSIX,
    -- | The time when the environment template was last modified.
    lastModifiedAt :: Core.POSIX,
    -- | The name of the environment template.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentTemplateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendedVersion', 'environmentTemplateSummary_recommendedVersion' - The recommended version of the environment template.
--
-- 'provisioning', 'environmentTemplateSummary_provisioning' - When included, indicates that the environment template is for customer
-- provisioned and managed infrastructure.
--
-- 'displayName', 'environmentTemplateSummary_displayName' - The name of the environment template as displayed in the developer
-- interface.
--
-- 'description', 'environmentTemplateSummary_description' - A description of the environment template.
--
-- 'arn', 'environmentTemplateSummary_arn' - The Amazon Resource Name (ARN) of the environment template.
--
-- 'createdAt', 'environmentTemplateSummary_createdAt' - The time when the environment template was created.
--
-- 'lastModifiedAt', 'environmentTemplateSummary_lastModifiedAt' - The time when the environment template was last modified.
--
-- 'name', 'environmentTemplateSummary_name' - The name of the environment template.
newEnvironmentTemplateSummary ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModifiedAt'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  EnvironmentTemplateSummary
newEnvironmentTemplateSummary
  pArn_
  pCreatedAt_
  pLastModifiedAt_
  pName_ =
    EnvironmentTemplateSummary'
      { recommendedVersion =
          Prelude.Nothing,
        provisioning = Prelude.Nothing,
        displayName = Prelude.Nothing,
        description = Prelude.Nothing,
        arn = pArn_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        lastModifiedAt =
          Core._Time Lens.# pLastModifiedAt_,
        name = pName_
      }

-- | The recommended version of the environment template.
environmentTemplateSummary_recommendedVersion :: Lens.Lens' EnvironmentTemplateSummary (Prelude.Maybe Prelude.Text)
environmentTemplateSummary_recommendedVersion = Lens.lens (\EnvironmentTemplateSummary' {recommendedVersion} -> recommendedVersion) (\s@EnvironmentTemplateSummary' {} a -> s {recommendedVersion = a} :: EnvironmentTemplateSummary)

-- | When included, indicates that the environment template is for customer
-- provisioned and managed infrastructure.
environmentTemplateSummary_provisioning :: Lens.Lens' EnvironmentTemplateSummary (Prelude.Maybe Provisioning)
environmentTemplateSummary_provisioning = Lens.lens (\EnvironmentTemplateSummary' {provisioning} -> provisioning) (\s@EnvironmentTemplateSummary' {} a -> s {provisioning = a} :: EnvironmentTemplateSummary)

-- | The name of the environment template as displayed in the developer
-- interface.
environmentTemplateSummary_displayName :: Lens.Lens' EnvironmentTemplateSummary (Prelude.Maybe Prelude.Text)
environmentTemplateSummary_displayName = Lens.lens (\EnvironmentTemplateSummary' {displayName} -> displayName) (\s@EnvironmentTemplateSummary' {} a -> s {displayName = a} :: EnvironmentTemplateSummary) Prelude.. Lens.mapping Core._Sensitive

-- | A description of the environment template.
environmentTemplateSummary_description :: Lens.Lens' EnvironmentTemplateSummary (Prelude.Maybe Prelude.Text)
environmentTemplateSummary_description = Lens.lens (\EnvironmentTemplateSummary' {description} -> description) (\s@EnvironmentTemplateSummary' {} a -> s {description = a} :: EnvironmentTemplateSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) of the environment template.
environmentTemplateSummary_arn :: Lens.Lens' EnvironmentTemplateSummary Prelude.Text
environmentTemplateSummary_arn = Lens.lens (\EnvironmentTemplateSummary' {arn} -> arn) (\s@EnvironmentTemplateSummary' {} a -> s {arn = a} :: EnvironmentTemplateSummary)

-- | The time when the environment template was created.
environmentTemplateSummary_createdAt :: Lens.Lens' EnvironmentTemplateSummary Prelude.UTCTime
environmentTemplateSummary_createdAt = Lens.lens (\EnvironmentTemplateSummary' {createdAt} -> createdAt) (\s@EnvironmentTemplateSummary' {} a -> s {createdAt = a} :: EnvironmentTemplateSummary) Prelude.. Core._Time

-- | The time when the environment template was last modified.
environmentTemplateSummary_lastModifiedAt :: Lens.Lens' EnvironmentTemplateSummary Prelude.UTCTime
environmentTemplateSummary_lastModifiedAt = Lens.lens (\EnvironmentTemplateSummary' {lastModifiedAt} -> lastModifiedAt) (\s@EnvironmentTemplateSummary' {} a -> s {lastModifiedAt = a} :: EnvironmentTemplateSummary) Prelude.. Core._Time

-- | The name of the environment template.
environmentTemplateSummary_name :: Lens.Lens' EnvironmentTemplateSummary Prelude.Text
environmentTemplateSummary_name = Lens.lens (\EnvironmentTemplateSummary' {name} -> name) (\s@EnvironmentTemplateSummary' {} a -> s {name = a} :: EnvironmentTemplateSummary)

instance Core.FromJSON EnvironmentTemplateSummary where
  parseJSON =
    Core.withObject
      "EnvironmentTemplateSummary"
      ( \x ->
          EnvironmentTemplateSummary'
            Prelude.<$> (x Core..:? "recommendedVersion")
            Prelude.<*> (x Core..:? "provisioning")
            Prelude.<*> (x Core..:? "displayName")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "lastModifiedAt")
            Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable EnvironmentTemplateSummary where
  hashWithSalt _salt EnvironmentTemplateSummary' {..} =
    _salt `Prelude.hashWithSalt` recommendedVersion
      `Prelude.hashWithSalt` provisioning
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastModifiedAt
      `Prelude.hashWithSalt` name

instance Prelude.NFData EnvironmentTemplateSummary where
  rnf EnvironmentTemplateSummary' {..} =
    Prelude.rnf recommendedVersion
      `Prelude.seq` Prelude.rnf provisioning
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModifiedAt
      `Prelude.seq` Prelude.rnf name
