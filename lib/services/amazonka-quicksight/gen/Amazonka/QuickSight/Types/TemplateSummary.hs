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
-- Module      : Amazonka.QuickSight.Types.TemplateSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TemplateSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The template summary.
--
-- /See:/ 'newTemplateSummary' smart constructor.
data TemplateSummary = TemplateSummary'
  { -- | A summary of a template.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The last time that this template was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The last time that this template was updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | A structure containing a list of version numbers for the template
    -- summary.
    latestVersionNumber :: Prelude.Maybe Prelude.Natural,
    -- | A display name for the template.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the template. This ID is unique per Amazon Web Services Region
    -- for each Amazon Web Services account.
    templateId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'templateSummary_arn' - A summary of a template.
--
-- 'createdTime', 'templateSummary_createdTime' - The last time that this template was created.
--
-- 'lastUpdatedTime', 'templateSummary_lastUpdatedTime' - The last time that this template was updated.
--
-- 'latestVersionNumber', 'templateSummary_latestVersionNumber' - A structure containing a list of version numbers for the template
-- summary.
--
-- 'name', 'templateSummary_name' - A display name for the template.
--
-- 'templateId', 'templateSummary_templateId' - The ID of the template. This ID is unique per Amazon Web Services Region
-- for each Amazon Web Services account.
newTemplateSummary ::
  TemplateSummary
newTemplateSummary =
  TemplateSummary'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      latestVersionNumber = Prelude.Nothing,
      name = Prelude.Nothing,
      templateId = Prelude.Nothing
    }

-- | A summary of a template.
templateSummary_arn :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.Text)
templateSummary_arn = Lens.lens (\TemplateSummary' {arn} -> arn) (\s@TemplateSummary' {} a -> s {arn = a} :: TemplateSummary)

-- | The last time that this template was created.
templateSummary_createdTime :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.UTCTime)
templateSummary_createdTime = Lens.lens (\TemplateSummary' {createdTime} -> createdTime) (\s@TemplateSummary' {} a -> s {createdTime = a} :: TemplateSummary) Prelude.. Lens.mapping Data._Time

-- | The last time that this template was updated.
templateSummary_lastUpdatedTime :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.UTCTime)
templateSummary_lastUpdatedTime = Lens.lens (\TemplateSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@TemplateSummary' {} a -> s {lastUpdatedTime = a} :: TemplateSummary) Prelude.. Lens.mapping Data._Time

-- | A structure containing a list of version numbers for the template
-- summary.
templateSummary_latestVersionNumber :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.Natural)
templateSummary_latestVersionNumber = Lens.lens (\TemplateSummary' {latestVersionNumber} -> latestVersionNumber) (\s@TemplateSummary' {} a -> s {latestVersionNumber = a} :: TemplateSummary)

-- | A display name for the template.
templateSummary_name :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.Text)
templateSummary_name = Lens.lens (\TemplateSummary' {name} -> name) (\s@TemplateSummary' {} a -> s {name = a} :: TemplateSummary)

-- | The ID of the template. This ID is unique per Amazon Web Services Region
-- for each Amazon Web Services account.
templateSummary_templateId :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.Text)
templateSummary_templateId = Lens.lens (\TemplateSummary' {templateId} -> templateId) (\s@TemplateSummary' {} a -> s {templateId = a} :: TemplateSummary)

instance Data.FromJSON TemplateSummary where
  parseJSON =
    Data.withObject
      "TemplateSummary"
      ( \x ->
          TemplateSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "LatestVersionNumber")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "TemplateId")
      )

instance Prelude.Hashable TemplateSummary where
  hashWithSalt _salt TemplateSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` latestVersionNumber
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` templateId

instance Prelude.NFData TemplateSummary where
  rnf TemplateSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf latestVersionNumber
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf templateId
