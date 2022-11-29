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
-- Module      : Amazonka.SSMSAP.Types.DatabaseSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMSAP.Types.DatabaseSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMSAP.Types.DatabaseType

-- |
--
-- /See:/ 'newDatabaseSummary' smart constructor.
data DatabaseSummary = DatabaseSummary'
  { tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    arn :: Prelude.Maybe Prelude.Text,
    databaseType :: Prelude.Maybe DatabaseType,
    databaseId :: Prelude.Maybe Prelude.Text,
    componentId :: Prelude.Maybe Prelude.Text,
    applicationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatabaseSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'databaseSummary_tags' -
--
-- 'arn', 'databaseSummary_arn' -
--
-- 'databaseType', 'databaseSummary_databaseType' -
--
-- 'databaseId', 'databaseSummary_databaseId' -
--
-- 'componentId', 'databaseSummary_componentId' -
--
-- 'applicationId', 'databaseSummary_applicationId' -
newDatabaseSummary ::
  DatabaseSummary
newDatabaseSummary =
  DatabaseSummary'
    { tags = Prelude.Nothing,
      arn = Prelude.Nothing,
      databaseType = Prelude.Nothing,
      databaseId = Prelude.Nothing,
      componentId = Prelude.Nothing,
      applicationId = Prelude.Nothing
    }

-- |
databaseSummary_tags :: Lens.Lens' DatabaseSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
databaseSummary_tags = Lens.lens (\DatabaseSummary' {tags} -> tags) (\s@DatabaseSummary' {} a -> s {tags = a} :: DatabaseSummary) Prelude.. Lens.mapping Lens.coerced

-- |
databaseSummary_arn :: Lens.Lens' DatabaseSummary (Prelude.Maybe Prelude.Text)
databaseSummary_arn = Lens.lens (\DatabaseSummary' {arn} -> arn) (\s@DatabaseSummary' {} a -> s {arn = a} :: DatabaseSummary)

-- |
databaseSummary_databaseType :: Lens.Lens' DatabaseSummary (Prelude.Maybe DatabaseType)
databaseSummary_databaseType = Lens.lens (\DatabaseSummary' {databaseType} -> databaseType) (\s@DatabaseSummary' {} a -> s {databaseType = a} :: DatabaseSummary)

-- |
databaseSummary_databaseId :: Lens.Lens' DatabaseSummary (Prelude.Maybe Prelude.Text)
databaseSummary_databaseId = Lens.lens (\DatabaseSummary' {databaseId} -> databaseId) (\s@DatabaseSummary' {} a -> s {databaseId = a} :: DatabaseSummary)

-- |
databaseSummary_componentId :: Lens.Lens' DatabaseSummary (Prelude.Maybe Prelude.Text)
databaseSummary_componentId = Lens.lens (\DatabaseSummary' {componentId} -> componentId) (\s@DatabaseSummary' {} a -> s {componentId = a} :: DatabaseSummary)

-- |
databaseSummary_applicationId :: Lens.Lens' DatabaseSummary (Prelude.Maybe Prelude.Text)
databaseSummary_applicationId = Lens.lens (\DatabaseSummary' {applicationId} -> applicationId) (\s@DatabaseSummary' {} a -> s {applicationId = a} :: DatabaseSummary)

instance Core.FromJSON DatabaseSummary where
  parseJSON =
    Core.withObject
      "DatabaseSummary"
      ( \x ->
          DatabaseSummary'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "DatabaseType")
            Prelude.<*> (x Core..:? "DatabaseId")
            Prelude.<*> (x Core..:? "ComponentId")
            Prelude.<*> (x Core..:? "ApplicationId")
      )

instance Prelude.Hashable DatabaseSummary where
  hashWithSalt _salt DatabaseSummary' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` databaseType
      `Prelude.hashWithSalt` databaseId
      `Prelude.hashWithSalt` componentId
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData DatabaseSummary where
  rnf DatabaseSummary' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf databaseType
      `Prelude.seq` Prelude.rnf databaseId
      `Prelude.seq` Prelude.rnf componentId
      `Prelude.seq` Prelude.rnf applicationId
