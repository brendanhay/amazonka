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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMSAP.Types.DatabaseSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMSAP.Types.DatabaseType

-- |
--
-- /See:/ 'newDatabaseSummary' smart constructor.
data DatabaseSummary = DatabaseSummary'
  { applicationId :: Prelude.Maybe Prelude.Text,
    arn :: Prelude.Maybe Prelude.Text,
    componentId :: Prelude.Maybe Prelude.Text,
    databaseId :: Prelude.Maybe Prelude.Text,
    databaseType :: Prelude.Maybe DatabaseType,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'applicationId', 'databaseSummary_applicationId' -
--
-- 'arn', 'databaseSummary_arn' -
--
-- 'componentId', 'databaseSummary_componentId' -
--
-- 'databaseId', 'databaseSummary_databaseId' -
--
-- 'databaseType', 'databaseSummary_databaseType' -
--
-- 'tags', 'databaseSummary_tags' -
newDatabaseSummary ::
  DatabaseSummary
newDatabaseSummary =
  DatabaseSummary'
    { applicationId = Prelude.Nothing,
      arn = Prelude.Nothing,
      componentId = Prelude.Nothing,
      databaseId = Prelude.Nothing,
      databaseType = Prelude.Nothing,
      tags = Prelude.Nothing
    }

databaseSummary_applicationId :: Lens.Lens' DatabaseSummary (Prelude.Maybe Prelude.Text)
databaseSummary_applicationId = Lens.lens (\DatabaseSummary' {applicationId} -> applicationId) (\s@DatabaseSummary' {} a -> s {applicationId = a} :: DatabaseSummary)

databaseSummary_arn :: Lens.Lens' DatabaseSummary (Prelude.Maybe Prelude.Text)
databaseSummary_arn = Lens.lens (\DatabaseSummary' {arn} -> arn) (\s@DatabaseSummary' {} a -> s {arn = a} :: DatabaseSummary)

databaseSummary_componentId :: Lens.Lens' DatabaseSummary (Prelude.Maybe Prelude.Text)
databaseSummary_componentId = Lens.lens (\DatabaseSummary' {componentId} -> componentId) (\s@DatabaseSummary' {} a -> s {componentId = a} :: DatabaseSummary)

databaseSummary_databaseId :: Lens.Lens' DatabaseSummary (Prelude.Maybe Prelude.Text)
databaseSummary_databaseId = Lens.lens (\DatabaseSummary' {databaseId} -> databaseId) (\s@DatabaseSummary' {} a -> s {databaseId = a} :: DatabaseSummary)

databaseSummary_databaseType :: Lens.Lens' DatabaseSummary (Prelude.Maybe DatabaseType)
databaseSummary_databaseType = Lens.lens (\DatabaseSummary' {databaseType} -> databaseType) (\s@DatabaseSummary' {} a -> s {databaseType = a} :: DatabaseSummary)

databaseSummary_tags :: Lens.Lens' DatabaseSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
databaseSummary_tags = Lens.lens (\DatabaseSummary' {tags} -> tags) (\s@DatabaseSummary' {} a -> s {tags = a} :: DatabaseSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DatabaseSummary where
  parseJSON =
    Data.withObject
      "DatabaseSummary"
      ( \x ->
          DatabaseSummary'
            Prelude.<$> (x Data..:? "ApplicationId")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "ComponentId")
            Prelude.<*> (x Data..:? "DatabaseId")
            Prelude.<*> (x Data..:? "DatabaseType")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DatabaseSummary where
  hashWithSalt _salt DatabaseSummary' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` componentId
      `Prelude.hashWithSalt` databaseId
      `Prelude.hashWithSalt` databaseType
      `Prelude.hashWithSalt` tags

instance Prelude.NFData DatabaseSummary where
  rnf DatabaseSummary' {..} =
    Prelude.rnf applicationId `Prelude.seq`
      Prelude.rnf arn `Prelude.seq`
        Prelude.rnf componentId `Prelude.seq`
          Prelude.rnf databaseId `Prelude.seq`
            Prelude.rnf databaseType `Prelude.seq`
              Prelude.rnf tags
