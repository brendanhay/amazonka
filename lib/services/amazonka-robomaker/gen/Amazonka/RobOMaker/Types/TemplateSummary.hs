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
-- Module      : Amazonka.RobOMaker.Types.TemplateSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.TemplateSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information for a template.
--
-- /See:/ 'newTemplateSummary' smart constructor.
data TemplateSummary = TemplateSummary'
  { -- | The Amazon Resource Name (ARN) of the template.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the template was
    -- created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The time, in milliseconds since the epoch, when the template was last
    -- updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The name of the template.
    name :: Prelude.Maybe Prelude.Text,
    -- | The version of the template that you\'re using.
    version :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'templateSummary_arn' - The Amazon Resource Name (ARN) of the template.
--
-- 'createdAt', 'templateSummary_createdAt' - The time, in milliseconds since the epoch, when the template was
-- created.
--
-- 'lastUpdatedAt', 'templateSummary_lastUpdatedAt' - The time, in milliseconds since the epoch, when the template was last
-- updated.
--
-- 'name', 'templateSummary_name' - The name of the template.
--
-- 'version', 'templateSummary_version' - The version of the template that you\'re using.
newTemplateSummary ::
  TemplateSummary
newTemplateSummary =
  TemplateSummary'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the template.
templateSummary_arn :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.Text)
templateSummary_arn = Lens.lens (\TemplateSummary' {arn} -> arn) (\s@TemplateSummary' {} a -> s {arn = a} :: TemplateSummary)

-- | The time, in milliseconds since the epoch, when the template was
-- created.
templateSummary_createdAt :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.UTCTime)
templateSummary_createdAt = Lens.lens (\TemplateSummary' {createdAt} -> createdAt) (\s@TemplateSummary' {} a -> s {createdAt = a} :: TemplateSummary) Prelude.. Lens.mapping Data._Time

-- | The time, in milliseconds since the epoch, when the template was last
-- updated.
templateSummary_lastUpdatedAt :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.UTCTime)
templateSummary_lastUpdatedAt = Lens.lens (\TemplateSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@TemplateSummary' {} a -> s {lastUpdatedAt = a} :: TemplateSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the template.
templateSummary_name :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.Text)
templateSummary_name = Lens.lens (\TemplateSummary' {name} -> name) (\s@TemplateSummary' {} a -> s {name = a} :: TemplateSummary)

-- | The version of the template that you\'re using.
templateSummary_version :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.Text)
templateSummary_version = Lens.lens (\TemplateSummary' {version} -> version) (\s@TemplateSummary' {} a -> s {version = a} :: TemplateSummary)

instance Data.FromJSON TemplateSummary where
  parseJSON =
    Data.withObject
      "TemplateSummary"
      ( \x ->
          TemplateSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable TemplateSummary where
  hashWithSalt _salt TemplateSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version

instance Prelude.NFData TemplateSummary where
  rnf TemplateSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf version
